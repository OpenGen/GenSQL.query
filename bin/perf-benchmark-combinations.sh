#!/usr/bin/env bash

# This iterates over all combinations of synthetic data+model params and
# benchmarks each one.
# NB: This can take a while, a few days at worst

# Check if at least one argument is provided
if [ $# -lt 1 ]; then
  echo "Usage: $0 <jar-file> [extra-params...]"
  exit 1
fi

# Extract the JAR file name from the first argument
jar_file="$1"
shift # Remove the first argument so that "$@" contains only the extra params

# Function to handle SIGINT (Ctrl+C)
cleanup() {
  set +x
  echo "Script interrupted. Exiting..."
  exit 1
}

# Trap SIGINT signal and call cleanup function
trap cleanup SIGINT

# Enable command echoing
set -x

# Params to iterate over
num_rows_array=(10 50 200 500 1000 10000)
num_columns_array=(5 10 20 50 100 500)
num_views_array=(5 10 20 50 100 200)
num_clusters_per_view_array=(5 10 20 50 100 200)

for num_rows in "${num_rows_array[@]}"; do
  for num_columns in "${num_columns_array[@]}"; do
    for num_views in "${num_views_array[@]}"; do
      for num_clusters_per_view in "${num_clusters_per_view_array[@]}"; do
        # Benchmark, but use "|| true" to continue on failure
        java -jar "$jar_file" \
          --synthetic \
          --num-rows "$num_rows" \
          --num-columns "$num_columns" \
          --num-views "$num_views" \
          --num-clusters-per-view "$num_clusters_per_view" \
          --no-overwrite \
          "$@" || true
      done
    done
  done
done

