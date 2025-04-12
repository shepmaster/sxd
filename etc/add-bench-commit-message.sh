#!/bin/bash

set -eu -o pipefail

echo "Benchmarking: $(git show -q '--pretty=format:%s')"

# Rotate previous files 

touch hyperfine.current
mv hyperfine.current hyperfine.prev

touch iai.current
mv iai.current iai.prev

# Perform current benchmarks

hyperfine -p 'cargo build --release' -w3 'QUIET=1 ./target/release/sxd ./standard.xml' > hyperfine.current

FILE=../standard.xml cargo bench -p sxd-validation --bench benchmark_iai > iai.current

# Do light number crunching

hyperfine_prev_time=$(awk '/Time/{if($6 == "s") print $5*1000; else print $5; }' < hyperfine.prev )
hyperfine_curr_time=$(awk '/Time/{if($6 == "s") print $5*1000; else print $5; }' < hyperfine.current)
hyperfine_percent=$(awk "BEGIN { printf \"%.2f\", 100 * ${hyperfine_curr_time:-0} / ${hyperfine_prev_time:-1} }")

cat <<MSG > commit-message
Benchmarking data

Before:

\`\`\`
$(grep -E 'Time|Range' < hyperfine.prev)
\`\`\`

After:

\`\`\`
$(grep -E 'Time|Range' < hyperfine.current)
\`\`\`

That's ${hyperfine_percent}% of the previous time.

\`\`\`
$(grep -E 'Instructions|L1 Accesses|Estimated Cycles' < iai.current)
\`\`\`
MSG

git commit --allow-empty -F commit-message
