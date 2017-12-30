--- @module Queue
-- @author Eric Mertens <emertens@gmail.com>
-- @copyright 2017 Eric Mertens
local Queue = {}

local methods = {}
local mt = { __name = 'Queue', __index = methods }

--- Predicate to check if queue can be popped.
-- @return boolean
function methods:ready ()
        return self.first <= self.last
end

--- Add a new value to the end of the queue.
-- @param val Value to add to queue
function methods:push (val)
        self.last = self.last + 1
        self[self.last] = val
end

--- Pop an element off the front of the queue.
-- @return Next value or nil
function methods:pop ()
        local res
        if self.first <= self.last then
                res = self[self.first]
                self[self.first] = nil
                self.first = self.first + 1
        end
        return res
end

--- Remove all values from queue.
function methods:reset ()
        for i = self.first, self.last do
                rawset(list, i, nil)
        end
        self.first = 1
        self.last = 0
end

--- Create a new queue initialized with the given values.
-- @param ... Initial values
function Queue.new (...)
        local list = {...}
        list.first = 1
        list.last = #list
        setmetatable(list, mt)
        return list
end

return Queue
