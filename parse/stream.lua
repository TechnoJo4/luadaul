-- lazy token stream
return function(func, arg)
    local last, stream, i = {}, {}, 0

    local self; self = {
        pos = 0,

        generate = function()
            if i > 0 and not last[2] then
                error("Stream error: tried to generate after end of stream")
            end
            i = i + 1
            last[1] = arg
            last = { func(unpack(last)) }
            stream[i] = last[1]
        end,
        advance = function()
            self.pos = self.pos + 1
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
        end
    }

    self.adv = self.advance
    return self
end
