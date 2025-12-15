local cjson = require("cjson.safe")

local M = {}

-- Read request body and decode JSON (returns table or nil, err)
function M.read_json_body()
    ngx.req.read_body()
    local body = ngx.req.get_body_data()
    if not body or body == "" then
        return nil, "empty_body"
    end
    local data, err = cjson.decode(body)
    if not data then
        return nil, "invalid_json"
    end
    return data, nil
end

-- Send successful JSON response
function M.ok(payload)
    ngx.status = ngx.HTTP_OK
    ngx.header["Content-Type"] = "application/json; charset=utf-8"
    ngx.say(cjson.encode(payload or { ok = true }))
    return ngx.exit(ngx.HTTP_OK)
end

-- Send error JSON response with given status and code
function M.error(status, code, message)
    ngx.status = status or ngx.HTTP_BAD_REQUEST
    ngx.header["Content-Type"] = "application/json; charset=utf-8"
    ngx.say(cjson.encode({
        ok = false,
        error = message or "error",
        code = code or "error",
    }))
    return ngx.exit(status or ngx.HTTP_BAD_REQUEST)
end

return M
