#!/bin/sh

set -e

if [ -z "$1" ]; then
	echo "Usage: $0 <day number>" >&2
	exit 1
fi

day="day$1"

if [ -d "$day" ]; then
	echo "Directory $day/ already exists" >&2
	exit 1
fi

for dir in 'in' out code build scripts; do
	mkdir -p "$day/$dir"
done

echo "# Day $1" > "$day/README.md"
touch "$day/build/.keep"
cat > "$day/scripts/build.sh" <<'EOF'
#!/bin/sh

set -e
cd "$(dirname "$0")"/../code
EOF
cat > "$day/scripts/run.sh" <<'EOF'
#!/bin/sh

set -e
cd "$(dirname "$0")"/..
EOF
chmod +x "${day}"/scripts/run.sh "${day}"/scripts/build.sh

echo "Successfully created skeleton directory for day $1"
