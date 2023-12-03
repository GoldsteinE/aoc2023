function s:neighbours(x, y, width, height)
    let result = []
    for offset_x in [-1, 0, 1]
        for offset_y in [-1, 0, 1]
            if a:x + offset_x < 0 || a:x + offset_x >= a:width
                break
            endif
            if a:y + offset_y < 0 || a:y + offset_y >= a:height
                continue
            endif
            call add(result, [a:x + offset_x, a:y + offset_y])
        endfor
    endfor
    return result
endfunction

let part = getenv('PART')
let input_file = getenv('INPUT')
let matrix = readfile(input_file)
let width = len(matrix)
let height = len(matrix[0])
let part1_result = 0
let gear_adjacency = {}
let y = 0
let x = 0
while y < height
    let line = matrix[y]
    let [num, start, end] = matchstrpos(line, "\\d\\+")
    while start != -1
        let found_good_neighbour = v:false
        let x = start
        while x < end
            for [nx, ny] in s:neighbours(x, y, width, height)
                if match(matrix[ny][nx], "[.0123456789]") == -1
                    let found_good_neighbour = v:true
                endif
                if matrix[ny][nx] == "*"
                    let gear_key = nx . "," . ny
                    if !has_key(gear_adjacency, gear_key)
                        let gear_adjacency[gear_key] = {}
                    endif
                    let gear_adjacency[gear_key][num] = 1
                endif
            endfor
            let x += 1
        endwhile
        if found_good_neighbour
            let part1_result += num
        endif
        let [num, start, end] = matchstrpos(line, "\\d\\+", end)
    endwhile
    let y += 1
endwhile

if part == 1
    echo part1_result . "\n"
else
    let part2_result = 0
    for [gear_key, numbers_dict] in items(gear_adjacency)
        let numbers = keys(numbers_dict)
        if len(numbers) == 2
            let [n, m] = numbers
            let part2_result += n * m
        endif
    endfor
    echo part2_result . "\n"
endif
