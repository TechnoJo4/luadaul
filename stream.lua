local mod = {}

mod.END = {}

mod.new = function(f)
	return setmetatable({ done = false, n = 0 }, { __index = function(self, i)
		if self.done and i > self.n then
			error("attempt to consume stream past completion")
			return
		end

		while self.n < i do
			self.n = self.n + 1
			self[self.n] = f()
			if self[self.n] == mod.END then
				self.done = true
				if i ~= self.n then
					error("attempt to consume stream past end")
				end
				break
			end
		end

		return self[i]
	end })
end

return mod
