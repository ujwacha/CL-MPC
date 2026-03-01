import socket
import struct
import threading
import time

HOST = 'localhost'
PORT = 9999

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((HOST, PORT))
sock.settimeout(0.1)

client_addr = None
auto_send = False
auto_interval = 0.1  # 10 Hz
send_values = [1.0, 2.0, 3.0, 4.0]

def pack_floats(floats):
    return struct.pack('<' + 'f' * len(floats), *floats)

def unpack_floats(data):
    count = len(data) // 4
    return struct.unpack('<' + 'f' * count, data[:count * 4])

def receiver():
    global client_addr
    while True:
        try:
            data, addr = sock.recvfrom(1024)
            client_addr = addr
            floats = unpack_floats(data)
            print(f"\n[RX from {addr}] {list(floats)}")
            print("> ", end='', flush=True)
        except socket.timeout:
            pass

def auto_sender():
    while True:
        if auto_send and client_addr:
            send_now(send_values, silent=True)
        time.sleep(auto_interval)

def send_now(values, silent=False):
    if client_addr is None:
        print("No client connected yet.")
        return
    data = pack_floats(values)
    sock.sendto(data, client_addr)
    if not silent:
        print(f"[TX to {client_addr}] {values}")

threading.Thread(target=receiver, daemon=True).start()
threading.Thread(target=auto_sender, daemon=True).start()

print(f"UDP server listening on {HOST}:{PORT}")
print("Commands:")
print("  s              - send current values once")
print("  s 1.1 2.2 ...  - send custom floats once")
print("  set 1.1 2.2 .. - update the auto-send values")
print("  auto on/off    - toggle 10Hz auto-send")
print("  q              - quit")
print()

while True:
    try:
        cmd = input("> ").strip()
    except (EOFError, KeyboardInterrupt):
        print("\nBye.")
        break

    if not cmd:
        continue
    elif cmd == 'q':
        break
    elif cmd == 'auto on':
        auto_send = True
        print(f"Auto-send ON at 10Hz: {send_values}")
    elif cmd == 'auto off':
        auto_send = False
        print("Auto-send OFF")
    elif cmd.startswith('set '):
        try:
            send_values = [float(x) for x in cmd[4:].split()]
            print(f"Auto-send values set to: {send_values}")
        except ValueError:
            print("Bad values.")
    elif cmd == 's':
        send_now(send_values)
    elif cmd.startswith('s '):
        try:
            vals = [float(x) for x in cmd[2:].split()]
            send_now(vals)
        except ValueError:
            print("Bad values.")
    else:
        print("Unknown command.")

sock.close()
