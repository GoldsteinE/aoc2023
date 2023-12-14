@enum Cell :: UInt8 begin
    Empty = Int('.')
    Square = Int('#')
    Round = Int('O')
end

function tilt!(m :: Matrix{Cell}, delta :: CartesianIndex{2})
    w, h = size(m)
    progress = true
    while progress
        progress = false
        for xy in keys(m)
            m[xy] ≠ Round && continue
            nxy = xy + delta
            if checkbounds(Bool, m, nxy) && m[nxy] == Empty
                progress = true
                m[nxy], m[xy] = Round, Empty
            end
        end
    end
end

function main()
    m = hcat((line -> Cell.(transcode(UInt8, line))).(readlines(stdin))...)
    directions = CartesianIndex.([(0, -1), (-1, 0), (0, 1), (1, 0)])
    seen = Dict{Tuple{Int64, Matrix{Cell}}, Int64}()
    n = 0
    limit = ARGS[1] == "2" ? 4_000_000_000 : 1
    while n < limit
        dir_idx = 1 + n % 4
        if haskey(seen, (dir_idx, m))
            diff = n - seen[(dir_idx, m)]
            n += (limit - n) ÷ diff * diff
        else
            seen[(dir_idx, copy(m))] = n
        end
        tilt!(m, directions[dir_idx])
        n += 1
    end

    _, h = size(m)
    println(sum(h - xy[2] + 1 for xy ∈ keys(m) if m[xy] == Round))
end

main()

# vim: ts=4 sw=4 et
