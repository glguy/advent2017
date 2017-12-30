--- @module day18
-- @copyright 2017 Eric Mertens
-- @author Eric Mertens <emertens@gmail.com>

local Queue = require 'Queue'

--- Split string into whitespace-delimited words.
-- @param s string to split
-- @return array of words
local function split_words (s)
        local t = {}
        for x in s:gmatch("%S+") do table.insert(t,x) end
        return t
end

--- Load the program from file.
-- Each program line is loaded as an array of words in the line.
-- @param filename Filename of the program
-- @return array of program lines
local function load_program (filename)
        local t = {}
        for v in io.lines '../inputs/input18.txt' do
                table.insert(t, split_words(v))
        end
        return t
end

local program = load_program '../inputs/input18.txt'

--- Construct the value suitable for initializing the run function.
-- @param program_id Initial value of register p
-- @return valid argument to run function
-- @see run
local function initialize (program_id)
        return { registers = { p = program_id }, counters = {} }
end

--- Run the program given some initial values as built by initialize
--
-- This function is intended to be run as the body of a coroutine.
-- It will yield one of: 'send', 'recv', or 'done'
--
-- 'send' yields the sent value
-- 'recv' yields the old register value and expects a new one
-- 'done' yields no values and indicates termination of the program
--
-- @param initial initial machine parameters
-- @see initialize
local function run (initial)
        local registers = initial.registers
        local counters  = initial.counters
        local pc        = 1

        local function val(r)
                return tonumber(r) or registers[r] or 0
        end

        local ops = {}
        function ops.snd (a1   )                 coroutine.yield('send', val(a1)) end
        function ops.rcv (a1   ) registers[a1] = coroutine.yield('recv', val(a1)) end
        function ops.set (a1,a2) registers[a1] = val(a2)                          end
        function ops.add (a1,a2) registers[a1] = val(a1) + val(a2)                end
        function ops.mul (a1,a2) registers[a1] = val(a1) * val(a2)                end
        function ops.mod (a1,a2) registers[a1] = val(a1) % val(a2)                end
        function ops.jgz (a1,a2) if val(a1) > 0 then pc = pc - 1 + val(a2) end    end

        while program[pc] do
                local op,a1,a2 = table.unpack(program[pc])
                counters[op] = (counters[op] or 0) + 1
                pc = pc + 1
                ops[op](a1,a2)
        end

        return 'done'
end

--- Compute the answer to part 1.
-- This runs the program while remembering send values until a non-zero
-- receive is executed.
-- @return Most recent send value at first non-zero receive
local function part_1 ()

    local pgm = coroutine.create(run)
    local mode,arg,last_snd

    local function step (...)
            local success
            success,mode,arg = coroutine.resume(pgm, ...)
            if not success then error(mode) end
    end

    step(initialize())

    while mode ~= 'recv' or arg == 0 do
            if mode == 'send' then last_snd = arg end
            step(arg)
    end

    return last_snd
end

--- Run a blocked program until it blocks again.
--
-- Programs are coroutines that are expected to yield with one of
-- 'done', 'send', or 'recv'. 'recv' states are resumed with values
-- from queue_in. Values returned from 'send' are pushed onto
-- queue_out. Execution halts either due to 'recv' with an empty
-- queue_in, or due to 'done'
--
-- @param pgm Coroutine
-- @param queue_in Values queued to be fed into *receive* commands
-- @param queue_out Values accumulated from *send* commands
local function drive (pgm, queue_in, queue_out)

        -- When program is finished, just flush its input queue
        if coroutine.status(pgm) == dead then
                queue_in:reset()
                return
        end

        -- otherwise the machine is blocked in receive mode
        local mode, val = 'recv', nil

        local function step (...)
                local success
                success, mode, val = coroutine.resume(pgm, ...)
                if not success then error(mode) end
        end

        while mode == 'recv' and queue_in:ready() do
                step(queue_in:pop())
                while mode == 'send' do
                        queue_out:push(val)
                        step()
                end
        end
end

--- Compute the answer to part 2
--
-- This runs two programs with IDs 0 and 1 and passes the values sent
-- by one program to the other via when the other attempts to receive.
-- Values are queued in order to support multiple sends before the other
-- program is ready to receive.
--
-- @return Number of sends performed by program with ID 1
local function part_2 ()
        local initial1 = initialize(0)
        local pgm1     = coroutine.create(run)
        local queue1   = Queue.new(initial1)

        local initial2 = initialize(1)
        local pgm2     = coroutine.create(run)
        local queue2   = Queue.new(initial2)

        while queue1:ready() or queue2:ready() do
                drive(pgm1, queue1, queue2)
                drive(pgm2, queue2, queue1)
        end

        return initial2.counters.snd
end

print('Part 1: ' .. part_1())
print('Part 2: ' .. part_2())
