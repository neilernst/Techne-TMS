#!/usr/bin/env python3
"""Script to convert .techne CommonLisp test files to Clojure"""

import re
import os
from pathlib import Path

def convert_techne_to_clojure(input_file, output_file):
    """Convert a single .techne file to Clojure"""

    with open(input_file, 'r') as f:
        lines = f.readlines()

    output_lines = []
    in_setf = False
    node_defs = []

    for i, line in enumerate(lines):
        # Keep comment lines
        if line.strip().startswith(';;'):
            if i == 0:
                output_lines.append(line)
            continue

        # Skip load statements
        if '(load ' in line:
            continue

        # Skip defvar line
        if 'defvar *rekb*' in line:
            continue

        # Start of setf block
        if line.strip().startswith('(setf') or '(setf' in line:
            in_setf = True
            # Check if first node is on same line as setf
            match = re.search(r'(node-\d+)\s+\(declare-atomic nil "(.+?)" :(\w+) \*rekb\*\)', line)
            if match:
                node_name, label, sort = match.groups()
                node_defs.append(f'(def {node_name} (declare-atomic nil "{label}" :{sort} rekb))\n')
            continue

        # End of setf block (lone closing paren)
        if in_setf and line.strip() == ')':
            in_setf = False
            continue

        # Collect node definitions within setf
        if in_setf:
            match = re.search(r'(node-\d+)\s+\(declare-atomic nil "(.+?)" :(\w+) \*rekb\*\)', line)
            if match:
                node_name, label, sort = match.groups()
                node_defs.append(f'(def {node_name} (declare-atomic nil "{label}" :{sort} rekb))\n')
            continue

        # Convert assrt-formula lines
        if 'assrt-formula' in line:
            # Replace list with vector syntax
            converted = line.replace('(list ', '[')
            # Replace *rekb* with rekb
            converted = converted.replace('*rekb*', 'rekb')
            # Fix closing parens for lists
            converted = re.sub(r'\)(\s+:DA)', r']\1', converted)
            output_lines.append(converted)

    # Write output file
    with open(output_file, 'w') as f:
        # Write header
        f.write(output_lines[0])  # Comment line
        f.write("(require '[techne-psm :refer :all])\n\n")
        f.write('(def rekb (create-rekb "Random rekb"))\n\n')

        # Write node definitions
        for node_def in node_defs:
            f.write(node_def)
        f.write('\n')

        # Write assrt-formula calls
        for line in output_lines[1:]:
            f.write(line)

def main():
    examples_dir = Path('examples')
    output_dir = Path('examples/clojure')

    # Find all .techne files
    techne_files = list(examples_dir.glob('*.techne'))

    print(f"Found {len(techne_files)} .techne files to convert")

    for techne_file in techne_files:
        # Skip rand-50 since we already converted it manually
        if techne_file.stem == 'rand-50':
            print(f"Skipping {techne_file.name} (already converted)")
            continue

        output_file = output_dir / f"{techne_file.stem}.clj"
        print(f"Converting {techne_file.name} -> {output_file.name}")

        try:
            convert_techne_to_clojure(techne_file, output_file)
        except Exception as e:
            print(f"  Error: {e}")

    print("\nConversion complete!")

if __name__ == '__main__':
    main()
