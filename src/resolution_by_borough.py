"""Share of NYC 311 requests resolved within 7 days, by borough.

Resolved within 7 days := closed_date is present and (closed_date - created_date) <= 7 days.
Denominator = every request logged in that borough, so requests still open (or never
closed) count as *not* resolved within 7 days.
"""

import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter

CSV = os.path.join("data", "nyc_311_sample.csv")
OUT = os.path.join("output", "311-resolved-7days-by-borough.png")

# ---- palette (reference data-viz palette, light mode) ----
SURFACE = "#fcfcfb"
SERIES = "#2a78d6"
INK = "#0b0b0b"
INK_2 = "#52514e"
MUTED = "#898781"
GRID = "#e1e0d9"
AXIS = "#c3c2b7"

plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "font.size": 10,
})

# ---------- data ----------
df = pd.read_csv(CSV, low_memory=False)
created = pd.to_datetime(df.created_date, errors="coerce")
closed = pd.to_datetime(df.closed_date, errors="coerce")
df["days"] = (closed - created).dt.total_seconds() / 86400

bad = df.days < 0                      # 3 records closed before they were created
df = df[~bad].copy()

df["within7"] = df.days.notna() & (df.days <= 7)

g = df.groupby("borough").agg(n=("within7", "size"), k=("within7", "sum"))
g["p"] = g.k / g.n

# Wilson 95% interval — the sample is small in Staten Island, so show the uncertainty
z = 1.96
den = 1 + z**2 / g.n
centre = (g.p + z**2 / (2 * g.n)) / den
half = z * np.sqrt(g.p * (1 - g.p) / g.n + z**2 / (4 * g.n**2)) / den
g["lo"], g["hi"] = centre - half, centre + half

g = g.sort_values("p", ascending=False)
citywide = df.within7.mean()

labels = [b.title() for b in g.index]
vals = g.p.values * 100
lo, hi = g.lo.values * 100, g.hi.values * 100

# ---------- chart ----------
fig, ax = plt.subplots(figsize=(7.6, 4.6), dpi=200)
fig.patch.set_facecolor(SURFACE)
ax.set_facecolor(SURFACE)

x = np.arange(len(labels))
# one measure, one colour — hue carries no information here, so it must not vary by rank
ax.bar(x, vals, width=0.62, color=SERIES, zorder=3)

# 95% CI whiskers, drawn thin and on top of the bars
ax.errorbar(x, vals, yerr=[vals - lo, hi - vals], fmt="none",
            ecolor=INK_2, elinewidth=1.2, capsize=5, capthick=1.2, zorder=4, alpha=0.75)

# citywide reference
ax.axhline(citywide * 100, color=AXIS, lw=1, ls=(0, (4, 4)), zorder=2)
ax.annotate(f"Citywide\n{citywide*100:.1f}%", xy=(len(labels) - 0.35, citywide * 100),
            ha="left", va="center", color=MUTED, fontsize=9, linespacing=1.35, zorder=5)

# direct value labels above each whisker
for xi, v, h in zip(x, vals, hi):
    ax.annotate(f"{v:.1f}%", xy=(xi, h), xytext=(0, 6), textcoords="offset points",
                ha="center", va="bottom", color=INK, fontsize=10.5, fontweight="bold")

# sample size under each borough name
for xi, (lab, n) in enumerate(zip(labels, g.n.values)):
    ax.annotate(f"n = {n:,}", xy=(xi, 0), xytext=(0, -26), textcoords="offset points",
                ha="center", va="top", color=MUTED, fontsize=8.5)

ax.set_xticks(x)
ax.set_xticklabels(labels, color=INK_2, fontsize=10.5)
ax.set_xlim(-0.6, len(labels) - 0.05)
ax.set_ylim(0, 100)
ax.set_yticks(np.arange(0, 101, 20))
ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:.0f}%"))
ax.set_ylabel("Requests resolved within 7 days", color=INK_2, fontsize=10.5, labelpad=9)

ax.set_title("Boroughs resolve 311 requests within a week at broadly similar rates",
             color=INK, fontsize=12.5, fontweight="bold", loc="left", pad=40)
ax.annotate("Share of logged requests closed within 7 days of creation, with 95% confidence\n"
            "intervals. NYC 311 sample, 4,823 requests created Jan 2024 – Dec 2025.",
            xy=(0, 1.015), xycoords="axes fraction", ha="left", va="bottom",
            color=INK_2, fontsize=9, linespacing=1.5)

ax.grid(axis="y", color=GRID, lw=0.8, zorder=0)
ax.set_axisbelow(True)
for side in ("top", "right", "left"):
    ax.spines[side].set_visible(False)
ax.spines["bottom"].set_color(AXIS)
ax.spines["bottom"].set_linewidth(1)
ax.tick_params(colors=MUTED, length=0, pad=6)

fig.tight_layout()
fig.savefig(OUT, facecolor=SURFACE, bbox_inches="tight", pad_inches=0.28)
print("saved", OUT)
print(g.assign(pct=(g.p * 100).round(1))[["n", "k", "pct"]].to_string())
