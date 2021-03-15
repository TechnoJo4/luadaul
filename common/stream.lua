-- lazy token stream
return function(func, arg, pregen)
    local last, stream, i = {}, {}, 0

    local time = 0
    if pregen then
        local s = os.clock()
        repeat
            i = i + 1
            last[1] = arg
            last = { func(unpack(last)) }
            stream[i] = last[1]
        until not last[2]
        time = os.clock() - s
    end

    local self; self = {
        pos = 0,
        source = arg,

        generate = function()
            if i > 0 and not last[2] then
                error("tried to generate after end of stream")
            end
            i = i + 1
            last[1] = arg
            last = { func(unpack(last)) }
            stream[i] = last[1]
        end,
        advance = function(amount)
            self.pos = self.pos + (amount or 1)
            while not stream[self.pos] do
                self.generate()
            end

            return stream[self.pos]
        end,
        recede = function(amount) -- "fuck fuck go back"
            self.pos = self.pos - (amount or 1)
            return stream[self.pos]
        end,
        get = function(offset)
            while not stream[self.pos + offset] do
                self.generate()
            end
            return stream[self.pos + offset]
        end,

        time = function()
            return time
        end
    }

    self.adv = self.advance
    return self
end
