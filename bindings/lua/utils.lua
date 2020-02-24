local ffi = require("ffi")

local function cstr(s)
    local c_str = ffi.new("char[?]", #s)
    ffi.copy(c_str, s)
    return c_str
end

local M = {}

M.cstr = cstr

return M
