local cjson = require("cjson.safe")

local M = {}

local DEFAULT_NATS_URL = "nats://nats:4222"
local ROUTER_SUBJECT = "beamline.router.v1.decide"

local function parse_nats_url(url)
    url = url or DEFAULT_NATS_URL
    local host, port = url:match("^nats://([^:]+):(%d+)$")
    if not host or not port then
        return "nats", 4222
    end
    return host, tonumber(port)
end

local function connect_nats()
    local host, port = parse_nats_url(os.getenv("NATS_URL"))

    local sock, err = ngx.socket.tcp()
    if not sock then
        return nil, "socket_create_failed: " .. (err or "unknown")
    end

    sock:settimeout(5000)

    local ok, cerr = sock:connect(host, port)
    if not ok then
        return nil, "connect_failed: " .. (cerr or "unknown")
    end

    -- Read initial INFO line (ignore contents)
    local _ = sock:receive("*l")

    -- Send CONNECT frame (simple, no auth)
    local connect_frame = "CONNECT {\"verbose\":false,\"pedantic\":false,\"tls_required\":false}\r\n"
    ok, cerr = sock:send(connect_frame)
    if not ok then
        sock:close()
        return nil, "connect_frame_failed: " .. (cerr or "unknown")
    end

    return sock
end

local function random_inbox()
    -- Simple inbox value; good enough for local dev
    return "_INBOX." .. tostring(ngx.now()) .. "." .. tostring(math.random(1, 1000000))
end

local function nats_request(subject, payload)
    local sock, err = connect_nats()
    if not sock then
        return nil, err
    end

    local inbox = random_inbox()

    local ok, serr

    -- Subscribe to reply inbox
    ok, serr = sock:send("SUB " .. inbox .. " 1\r\n")
    if not ok then
        sock:close()
        return nil, "sub_failed: " .. (serr or "unknown")
    end

    -- Publish request with reply inbox
    local frame = "PUB " .. subject .. " " .. inbox .. " " .. #payload .. "\r\n" .. payload .. "\r\n"
    ok, serr = sock:send(frame)
    if not ok then
        sock:close()
        return nil, "pub_failed: " .. (serr or "unknown")
    end

    -- Read lines until we get MSG for our inbox
    while true do
        local line, rerr = sock:receive("*l")
        if not line then
            sock:close()
            return nil, "recv_failed: " .. (rerr or "eof")
        end

        if line:sub(1, 4) == "PING" then
            sock:send("PONG\r\n")
        elseif line:sub(1, 3) == "+OK" then
            -- ignore
        elseif line:sub(1, 1) == "-" then
            sock:close()
            return nil, "nats_error: " .. line
        elseif line:sub(1, 3) == "MSG" then
            -- Format: MSG <subject> <sid> [reply-to] <#bytes>
            local _, _, msg_subject, _sid, reply_to, num_bytes = line:find("^MSG%s+(%S+)%s+(%S+)%s+(%S+)%s+(%d+)")
            if not msg_subject then
                _, _, msg_subject, _sid, num_bytes = line:find("^MSG%s+(%S+)%s+(%S+)%s+(%d+)")
                reply_to = nil
            end

            num_bytes = tonumber(num_bytes)
            if msg_subject ~= inbox then
                -- Not our inbox, skip payload
                sock:receive(num_bytes + 2) -- payload + CRLF
            else
                local data, perr = sock:receive(num_bytes)
                -- consume trailing CRLF
                sock:receive(2)
                sock:close()
                if not data then
                    return nil, "payload_recv_failed: " .. (perr or "unknown")
                end
                return data, nil
            end
        end
    end
end

function M.decide(request)
    local encoded, jerr = cjson.encode(request or {})
    if not encoded then
        return nil, "encode_failed: " .. (jerr or "unknown")
    end

    local resp, err = nats_request(ROUTER_SUBJECT, encoded)
    if not resp then
        return nil, err
    end

    local data, derr = cjson.decode(resp)
    if not data then
        return nil, "decode_failed: " .. (derr or "unknown")
    end

    return data, nil
end

return M
