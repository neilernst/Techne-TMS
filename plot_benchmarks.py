#!/usr/bin/env python3
"""
ATMS Performance Benchmark Visualization

Reads benchmark results from CSV and generates visualizations:
1. Bar chart comparing sequential vs parallel execution times
2. Speedup chart showing parallel performance gain
3. Statistical analysis with error bars
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import sys
import os
from pathlib import Path

# Set style
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (14, 10)
plt.rcParams['font.size'] = 10

def load_results(csv_file):
    """Load benchmark results from CSV file"""
    if not os.path.exists(csv_file):
        print(f"Error: File not found: {csv_file}")
        sys.exit(1)

    df = pd.read_csv(csv_file)
    print(f"\nLoaded {len(df)} results from {csv_file}")
    print(f"Benchmarks: {df['benchmark'].nunique()}")
    print(f"Configurations: {df['config'].unique()}")
    print(f"Runs per configuration: {df['run'].nunique()}")
    return df

def compute_statistics(df):
    """Compute mean and std for each benchmark and configuration"""
    stats = df.groupby(['benchmark', 'description', 'config'])['time_ms'].agg(['mean', 'std', 'count']).reset_index()

    # Compute speedup
    speedup_data = []
    for benchmark in stats['benchmark'].unique():
        bench_stats = stats[stats['benchmark'] == benchmark]
        seq_mean = bench_stats[bench_stats['config'] == 'sequential']['mean'].values[0]
        par_mean = bench_stats[bench_stats['config'] == 'parallel']['mean'].values[0]
        speedup = seq_mean / par_mean
        description = bench_stats['description'].values[0]

        speedup_data.append({
            'benchmark': benchmark,
            'description': description,
            'sequential_ms': seq_mean,
            'parallel_ms': par_mean,
            'speedup': speedup
        })

    speedup_df = pd.DataFrame(speedup_data)
    return stats, speedup_df

def plot_execution_times(stats, output_dir):
    """Create bar chart comparing execution times"""
    fig, ax = plt.subplots(figsize=(14, 8))

    benchmarks = stats['benchmark'].unique()
    x = range(len(benchmarks))
    width = 0.35

    seq_means = []
    seq_stds = []
    par_means = []
    par_stds = []

    for benchmark in benchmarks:
        bench_stats = stats[stats['benchmark'] == benchmark]
        seq_data = bench_stats[bench_stats['config'] == 'sequential']
        par_data = bench_stats[bench_stats['config'] == 'parallel']

        seq_means.append(seq_data['mean'].values[0])
        seq_stds.append(seq_data['std'].values[0])
        par_means.append(par_data['mean'].values[0])
        par_stds.append(par_data['std'].values[0])

    # Create bars
    bars1 = ax.bar([i - width/2 for i in x], seq_means, width,
                   label='Sequential', color='#FF6B6B', yerr=seq_stds,
                   capsize=5, alpha=0.8)
    bars2 = ax.bar([i + width/2 for i in x], par_means, width,
                   label='Parallel', color='#4ECDC4', yerr=par_stds,
                   capsize=5, alpha=0.8)

    # Customize plot
    ax.set_xlabel('Benchmark', fontweight='bold', fontsize=12)
    ax.set_ylabel('Execution Time (ms)', fontweight='bold', fontsize=12)
    ax.set_title('ATMS Benchmark: Sequential vs Parallel Execution Time\n(Lower is Better)',
                 fontweight='bold', fontsize=14, pad=20)
    ax.set_xticks(x)
    ax.set_xticklabels(benchmarks, rotation=45, ha='right')
    ax.legend(loc='upper left', fontsize=11)
    ax.grid(axis='y', alpha=0.3)

    # Add value labels on bars
    def autolabel(bars, values):
        for bar, val in zip(bars, values):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{val:.1f}ms',
                   ha='center', va='bottom', fontsize=8, rotation=0)

    autolabel(bars1, seq_means)
    autolabel(bars2, par_means)

    plt.tight_layout()
    output_file = os.path.join(output_dir, 'execution_times.png')
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_file}")
    plt.close()

def plot_speedup(speedup_df, output_dir):
    """Create bar chart showing speedup factors"""
    fig, ax = plt.subplots(figsize=(14, 8))

    benchmarks = speedup_df['benchmark']
    speedups = speedup_df['speedup']

    # Color bars based on speedup (red < 1.0, yellow 1.0-1.5, green > 1.5)
    colors = []
    for s in speedups:
        if s < 1.0:
            colors.append('#FF6B6B')  # Red - slower
        elif s < 1.5:
            colors.append('#FFD93D')  # Yellow - modest speedup
        else:
            colors.append('#6BCF7F')  # Green - good speedup

    bars = ax.bar(range(len(benchmarks)), speedups, color=colors, alpha=0.8, edgecolor='black')

    # Add baseline line at 1.0x
    ax.axhline(y=1.0, color='red', linestyle='--', linewidth=2, label='Baseline (1.0x)', alpha=0.7)

    # Customize plot
    ax.set_xlabel('Benchmark', fontweight='bold', fontsize=12)
    ax.set_ylabel('Speedup Factor', fontweight='bold', fontsize=12)
    ax.set_title('ATMS Parallel Speedup\n(Higher is Better, >1.0x means parallel is faster)',
                 fontweight='bold', fontsize=14, pad=20)
    ax.set_xticks(range(len(benchmarks)))
    ax.set_xticklabels(benchmarks, rotation=45, ha='right')
    ax.legend(loc='upper left', fontsize=11)
    ax.grid(axis='y', alpha=0.3)

    # Add value labels on bars
    for i, (bar, speedup) in enumerate(zip(bars, speedups)):
        height = bar.get_height()
        ax.text(bar.get_x() + bar.get_width()/2., height,
               f'{speedup:.2f}x',
               ha='center', va='bottom', fontsize=9, fontweight='bold')

    plt.tight_layout()
    output_file = os.path.join(output_dir, 'speedup.png')
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_file}")
    plt.close()

def plot_comparison_table(speedup_df, output_dir):
    """Create detailed comparison table"""
    fig, ax = plt.subplots(figsize=(14, 10))
    ax.axis('tight')
    ax.axis('off')

    # Prepare table data
    table_data = []
    for _, row in speedup_df.iterrows():
        table_data.append([
            row['benchmark'],
            row['description'],
            f"{row['sequential_ms']:.2f}",
            f"{row['parallel_ms']:.2f}",
            f"{row['speedup']:.2f}x",
            "✓" if row['speedup'] > 1.0 else "✗"
        ])

    # Create table
    table = ax.table(cellText=table_data,
                    colLabels=['Benchmark', 'Description', 'Sequential (ms)',
                              'Parallel (ms)', 'Speedup', 'Faster?'],
                    cellLoc='left',
                    loc='center',
                    colWidths=[0.15, 0.35, 0.12, 0.12, 0.10, 0.08])

    table.auto_set_font_size(False)
    table.set_fontsize(9)
    table.scale(1, 2)

    # Style header
    for i in range(6):
        cell = table[(0, i)]
        cell.set_facecolor('#4ECDC4')
        cell.set_text_props(weight='bold', color='white')

    # Color rows based on speedup
    for i, (_, row) in enumerate(speedup_df.iterrows(), 1):
        if row['speedup'] > 1.5:
            color = '#E8F8E8'  # Light green
        elif row['speedup'] > 1.0:
            color = '#FFF9E6'  # Light yellow
        else:
            color = '#FFE6E6'  # Light red

        for j in range(6):
            table[(i, j)].set_facecolor(color)

    plt.title('ATMS Benchmark Results - Detailed Comparison\n',
             fontweight='bold', fontsize=14, pad=20)

    output_file = os.path.join(output_dir, 'comparison_table.png')
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_file}")
    plt.close()

def plot_runtime_distribution(df, output_dir):
    """Create box plot showing runtime distribution"""
    fig, axes = plt.subplots(2, 1, figsize=(14, 10))

    # Sequential runtimes
    seq_data = df[df['config'] == 'sequential']
    sns.boxplot(data=seq_data, x='benchmark', y='time_ms', ax=axes[0], color='#FF6B6B')
    axes[0].set_title('Sequential Runtime Distribution', fontweight='bold', fontsize=12)
    axes[0].set_xlabel('Benchmark', fontweight='bold')
    axes[0].set_ylabel('Time (ms)', fontweight='bold')
    axes[0].tick_params(axis='x', rotation=45)
    axes[0].grid(axis='y', alpha=0.3)

    # Parallel runtimes
    par_data = df[df['config'] == 'parallel']
    sns.boxplot(data=par_data, x='benchmark', y='time_ms', ax=axes[1], color='#4ECDC4')
    axes[1].set_title('Parallel Runtime Distribution', fontweight='bold', fontsize=12)
    axes[1].set_xlabel('Benchmark', fontweight='bold')
    axes[1].set_ylabel('Time (ms)', fontweight='bold')
    axes[1].tick_params(axis='x', rotation=45)
    axes[1].grid(axis='y', alpha=0.3)

    plt.tight_layout()
    output_file = os.path.join(output_dir, 'runtime_distribution.png')
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved: {output_file}")
    plt.close()

def generate_summary_report(speedup_df, output_dir):
    """Generate text summary report"""
    report_lines = []
    report_lines.append("="*60)
    report_lines.append("ATMS Performance Benchmark Summary")
    report_lines.append("="*60)
    report_lines.append("")

    # Overall statistics
    avg_speedup = speedup_df['speedup'].mean()
    max_speedup = speedup_df['speedup'].max()
    min_speedup = speedup_df['speedup'].min()
    faster_count = len(speedup_df[speedup_df['speedup'] > 1.0])
    total_count = len(speedup_df)

    report_lines.append("Overall Statistics:")
    report_lines.append(f"  Average Speedup: {avg_speedup:.2f}x")
    report_lines.append(f"  Max Speedup:     {max_speedup:.2f}x")
    report_lines.append(f"  Min Speedup:     {min_speedup:.2f}x")
    report_lines.append(f"  Parallel Faster: {faster_count}/{total_count} benchmarks")
    report_lines.append("")

    # Per-benchmark results
    report_lines.append("Per-Benchmark Results:")
    report_lines.append("-"*60)
    for _, row in speedup_df.iterrows():
        report_lines.append(f"\n{row['benchmark']}:")
        report_lines.append(f"  Description: {row['description']}")
        report_lines.append(f"  Sequential:  {row['sequential_ms']:.2f} ms")
        report_lines.append(f"  Parallel:    {row['parallel_ms']:.2f} ms")
        report_lines.append(f"  Speedup:     {row['speedup']:.2f}x {'✓' if row['speedup'] > 1.0 else '✗'}")

    report_lines.append("")
    report_lines.append("="*60)
    report_lines.append("Note: Current implementation has NO actual parallelization yet.")
    report_lines.append("This is a BASELINE measurement. Speedup should be ~1.0x.")
    report_lines.append("="*60)

    report_text = "\n".join(report_lines)

    # Print to console
    print("\n" + report_text)

    # Save to file
    output_file = os.path.join(output_dir, 'summary_report.txt')
    with open(output_file, 'w') as f:
        f.write(report_text)
    print(f"\nSaved: {output_file}")

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 plot_benchmarks.py <csv_file>")
        print("Example: python3 plot_benchmarks.py benchmark_results.csv")
        sys.exit(1)

    csv_file = sys.argv[1]

    # Create output directory
    output_dir = "benchmark_plots"
    Path(output_dir).mkdir(exist_ok=True)

    print("\n" + "="*60)
    print("ATMS Benchmark Visualization")
    print("="*60)

    # Load and process data
    df = load_results(csv_file)
    stats, speedup_df = compute_statistics(df)

    print(f"\nGenerating plots in: {output_dir}/")

    # Generate all visualizations
    plot_execution_times(stats, output_dir)
    plot_speedup(speedup_df, output_dir)
    plot_comparison_table(speedup_df, output_dir)
    plot_runtime_distribution(df, output_dir)
    generate_summary_report(speedup_df, output_dir)

    print("\n" + "="*60)
    print("Visualization Complete!")
    print("="*60)
    print(f"\nAll files saved to: {output_dir}/")
    print("  - execution_times.png    : Bar chart of execution times")
    print("  - speedup.png            : Speedup factors")
    print("  - comparison_table.png   : Detailed comparison table")
    print("  - runtime_distribution.png: Runtime variability")
    print("  - summary_report.txt     : Text summary")

if __name__ == '__main__':
    main()
