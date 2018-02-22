#!/bin/bash

if (($# < 2)); then
    echo 'Need input and output as parameters!'
    exit
fi

input_file=$1
output_file=$2

if [ ${input_file#*.} == sid ]; then
    # Get start-of-data offset to strip header from sid file, add 1 for the tail index start pos.
    offset="$(($(od -s  -N 2 --endian=big -An -j 6 $input_file) + 1))"

    # Skip load address (if it is at the beginning of the data / not in header)
    if (($(od -s -N 2 -An -j 8 $input_file) == 0)); then
        let offset+=2
    fi
else
    offset=0 # No offset for non-sid binaries.
fi

# Add header
echo '#include <cytypes.h>' > $output_file

#Write array, don't use default name. (xxd get data from stdin)
echo -e "uint8 ${output_file%.*}[] = {\n$(tail -c +$offset $input_file | xxd -i)\n};" >> $output_file
