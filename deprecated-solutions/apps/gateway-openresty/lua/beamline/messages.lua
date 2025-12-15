-- HTTP handlers for message-related endpoints.
-- For now we only rely on util.lua and return simple stubs.

local util = require("beamline.util")
local router_client = require("beamline.router_client")

local M = {}

-- Example handler skeleton for future /api/v1/messages
function M.handle_messages()
    local body, err = util.read_json_body()
    if err then
        return util.error(ngx.HTTP_BAD_REQUEST, "invalid_request", err)
    end

    -- TODO: call router_client.decide(body) and map response to HTTP
    return util.ok({ ok = true, message = "stub", input = body })
end

return M
