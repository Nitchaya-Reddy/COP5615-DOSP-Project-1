#!/usr/bin/env bash
# Usage: ./calculate_time.sh ./program.exe [args...]

PROGRAM="$1"
shift
ARGS=("$@")

# Detect available cores
CORES=$(nproc 2>/dev/null || echo "N/A")

{ time "$PROGRAM" "${ARGS[@]}" ; } > program_output.txt 2> timing_output.txt

cat program_output.txt

real=$(grep real timing_output.txt | awk '{print $2}')
user=$(grep user timing_output.txt | awk '{print $2}')
sys=$(grep sys timing_output.txt | awk '{print $2}')

to_seconds() {
  local t="$1"
  if [[ "$t" =~ ([0-9]+)m([0-9]+\.[0-9]+)s ]]; then
    awk -v m="${BASH_REMATCH[1]}" -v s="${BASH_REMATCH[2]}" 'BEGIN{print m*60+s}'
  elif [[ "$t" =~ ([0-9]+\.[0-9]+)s ]]; then
    echo "${BASH_REMATCH[1]}"
  else
    echo "$t"
  fi
}

rt=$(to_seconds "$real")
ut=$(to_seconds "$user")
st=$(to_seconds "$sys")
cpu=$(awk -v u="$ut" -v s="$st" 'BEGIN{print u+s}')
ratio=$(awk -v c="$cpu" -v r="$rt" 'BEGIN{if (r>0) printf("%.2f", c/r); else print "NaN"}')
approx=$(awk -v x="$CORES" 'BEGIN{printf("%d", x)}')


gleam run

echo
echo
echo "parallelism with approximately $approx cores"
echo "Real Time: $rt seconds"
echo "User Time: $ut seconds"
echo "System Time: $st seconds"
echo "CPU Time: $cpu seconds"
echo "CPU Time to Real Time Ratio: $ratio"
echo "Available cores: $CORES"

