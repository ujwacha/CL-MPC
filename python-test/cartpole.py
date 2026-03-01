import socket
import struct
import threading
import time
import math

# --- Physics params (match your Lisp) ---
g = -9.8
r = 0.1          # pendulum length
dt = 0.01        # sim step (100Hz internally)
SEND_HZ = 50
SEND_EVERY = max(1, int((1/dt) / SEND_HZ))  # send every N sim steps

# --- State: [theta, omega, x, v] ---
state = [0.1, 0.0, 0.0, 0.0]   # small tilt to make it fall
a = 0.0                          # control input (cart acceleration)
running = True
sim_lock = threading.Lock()

# --- UDP ---
HOST = 'localhost'
PORT = 9999
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((HOST, PORT))
sock.settimeout(0.05)
client_addr = None

def pack_floats(floats):
    return struct.pack('<' + 'f' * len(floats), *floats)

def unpack_floats(data):
    count = len(data) // 4
    return struct.unpack('<' + 'f' * count, data[:count * 4])

def step_sim(s, ctrl):
    theta, omega, x, v = s
    inv_r2 = 1.0 / (r * r)
    d_theta = theta + omega * dt
    d_omega = omega + dt * inv_r2 * (g * math.sin(theta) + ctrl * math.cos(theta))
    d_x     = x + v * dt
    d_v     = v + ctrl * dt
    return [d_theta, d_omega, d_x, d_v]

def udp_receiver():
    global client_addr, a
    while running:
        try:
            data, addr = sock.recvfrom(1024)
            client_addr = addr
            floats = unpack_floats(data)
            if floats:
                with sim_lock:
                    a = floats[0]
                print(f"\n[RX] a = {floats[0]:.4f}  (from {addr})")
                print("> ", end='', flush=True)
        except socket.timeout:
            pass

def sim_loop():
    global state, client_addr
    step_count = 0
    while running:
        t0 = time.perf_counter()
        with sim_lock:
            cur_a = a
            state = step_sim(state, cur_a)
            cur_state = list(state)
        step_count += 1

        if step_count % SEND_EVERY == 0 and client_addr:
            sock.sendto(pack_floats(cur_state), client_addr)

        elapsed = time.perf_counter() - t0
        sleep_t = dt - elapsed
        if sleep_t > 0:
            time.sleep(sleep_t)

threading.Thread(target=udp_receiver, daemon=True).start()
threading.Thread(target=sim_loop, daemon=True).start()

print(f"Inverted Pendulum Sim  |  UDP {HOST}:{PORT}  |  TX {SEND_HZ}Hz")
print(f"State TX order : [theta, omega, x, v]")
print(f"Control RX     : [a]  (cart acceleration)")
print(f"Initial state  : theta=0.1 omega=0 x=0 v=0")
print()
print("Commands:")
print("  a <val>                    set control input")
print("  reset                      reset to default state")
print("  reset <theta> <w> <x> <v>  reset to custom state")
print("  state                      print current state")
print("  q                          quit")
print()

while True:
    try:
        cmd = input("> ").strip()
    except (EOFError, KeyboardInterrupt):
        running = False
        print("\nBye.")
        break

    if not cmd:
        continue
    elif cmd == 'q':
        running = False
        break
    elif cmd == 'state':
        with sim_lock:
            s = list(state)
        print(f"  theta={s[0]:.4f}  omega={s[1]:.4f}  x={s[2]:.4f}  v={s[3]:.4f}  a={a:.4f}")
    elif cmd.startswith('a '):
        try:
            with sim_lock:
                a = float(cmd[2:])
            print(f"  Control a = {a}")
        except ValueError:
            print("Bad value.")
    elif cmd == 'reset':
        with sim_lock:
            state = [0.1, 0.0, 0.0, 0.0]
            a = 0.0
        print("  Reset to default [0.1, 0, 0, 0]")
    elif cmd.startswith('reset '):
        try:
            vals = [float(x) for x in cmd[6:].split()]
            if len(vals) == 4:
                with sim_lock:
                    state = vals
                print(f"  Reset to {vals}")
            else:
                print("  Need 4 values: theta omega x v")
        except ValueError:
            print("Bad values.")
    else:
        print("Unknown command.")

sock.close()
