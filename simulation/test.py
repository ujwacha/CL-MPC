import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import socket
import struct
import time

# ----------------------------------------------------------------------
# Simulation parameters
# ----------------------------------------------------------------------
DT      = 0.02    # time step [s] (50 Hz)
L       = 1.0     # pendulum length [m]
G       = 9.81    # gravity [m/s²]
DAMPING = 0.1     # angular velocity damping

# Initial state
theta = 0.1
omega = 0.0
x     = 0.0
v     = 0.0

# ----------------------------------------------------------------------
# UDP setup (server on port 9999)
# ----------------------------------------------------------------------
UDP_IP   = ""
UDP_PORT = 9999
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind((UDP_IP, UDP_PORT))
sock.setblocking(False)
print(f"Listening for UDP commands (32-bit little-endian float) on port {UDP_PORT} ...")

client_addr = None   # set on first received packet, then kept forever
a = 0.0              # current acceleration command

# ----------------------------------------------------------------------
# Visualization setup
# ----------------------------------------------------------------------
VIEW_HALF   = 6.0        # half-width of the following window [m]  (total = 12 m)
cart_width  = 0.5
cart_height = 0.2

plt.ion()
fig, ax = plt.subplots(figsize=(9, 5))

# y limits: bob can be at most L above or below the pivot (cart_height),
# so [-0.9, 1.4] comfortably contains every possible bob position
ax.set_xlim(x - VIEW_HALF, x + VIEW_HALF)
ax.set_ylim(-L - 0.2, L + cart_height + 0.3)
ax.set_aspect('equal')
ax.grid(True, linestyle='--', alpha=0.4)
ax.set_title("Inverted Pendulum", fontsize=11)

cart_patch = patches.FancyBboxPatch(
    (x - cart_width / 2, 0), cart_width, cart_height,
    boxstyle="round,pad=0.02",
    linewidth=2, edgecolor='#2255cc', facecolor='#aaccff'
)
ax.add_patch(cart_patch)

# Horizontal rail hint (will be updated each frame)
rail_line, = ax.plot([], [], color='#888888', lw=2, zorder=0)

rod_line,  = ax.plot([], [], 'o-', lw=2.5, markersize=10,
                     color='#cc3322', solid_capstyle='round')

info_text = ax.text(0.02, 0.95, '', transform=ax.transAxes,
                    fontsize=9, verticalalignment='top',
                    fontfamily='monospace')

def update_plot(theta, omega, x, v, t):
    # ── Follow the cart: re-centre x window ──────────────────────────
    ax.set_xlim(x - VIEW_HALF, x + VIEW_HALF)

    # Rail drawn a bit wider than the view so it always looks continuous
    rail_y = cart_height / 2
    rail_line.set_data([x - VIEW_HALF - 1, x + VIEW_HALF + 1],
                       [rail_y, rail_y])

    # Cart
    cart_patch.set_x(x - cart_width / 2)

    # Rod and bob  (theta=0 → bob straight UP; theta=pi → bob straight DOWN)
    pivot_x = x
    pivot_y = cart_height
    bob_x   = pivot_x + L * np.sin(theta)
    bob_y   = pivot_y + L * np.cos(theta)
    rod_line.set_data([pivot_x, bob_x], [pivot_y, bob_y])

    info_text.set_text(
        f't={t:6.2f} s   x={x:7.3f} m   v={v:6.3f} m/s\n'
        f'θ={theta:7.4f} rad   ω={omega:7.4f} rad/s   a={a:6.3f} m/s²'
    )

# ----------------------------------------------------------------------
# Main loop
# ----------------------------------------------------------------------
t = 0.0
try:
    while True:
        # ── Receive UDP commands (non-blocking) ──────────────────────
        try:
            data, addr = sock.recvfrom(1024)
            if len(data) >= 4:
                if client_addr is None:
                    print(f"Client connected from {addr}")
                client_addr = addr
                try:
                    a = struct.unpack('<f', data[:4])[0]
                    print(a)
                except struct.error:
                    print(f"Warning: malformed packet from {addr} (len={len(data)})")
        except socket.error:
            pass   # no data available yet

        # ── Dynamics (Euler integration) ─────────────────────────────
        omega_dot = (G / L) * np.sin(theta) - (a / L) * np.cos(theta) - DAMPING * omega
        theta += omega * DT
        omega += omega_dot * DT
        theta  = (theta + np.pi) % (2 * np.pi) - np.pi
        x     += v * DT
        v     += a * DT
        t     += DT

        # ── Render ───────────────────────────────────────────────────
        update_plot(theta, omega, x, v, t)
        plt.pause(DT)

        # ── Send current state to client ─────────────────────────────
        if client_addr is not None:
            state_bytes = struct.pack('<ffff', theta, omega, x, v)
            try:
                sock.sendto(state_bytes, client_addr)
            except socket.error as e:
                print(f"Error sending to {client_addr}: {e}")

except KeyboardInterrupt:
    print("\nSimulation stopped.")
finally:
    sock.close()
    plt.ioff()
    plt.show()
