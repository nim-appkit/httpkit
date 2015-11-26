import strutils
import strtabs, tables
import utils/parser

type
  # See https://annevankesteren.nl/2007/10/http-methods.
  Method* {.pure.} = enum
    UNKNOWN

    # RFC 2616.
    OPTIONS
    GET
    HEAD
    POST
    PUT
    DELETE
    TRACE
    CONNECT

    # RFC 3253 (most omitted).
    UPDATE

    # draft-dusseault-http-patch
    PATCH


  Status* {.pure.} = enum
    UNKNOWN =                         (0, "Unknown")
    CONTINUE =                        (100, "Continue")
    SWITCHING_PROTOCOLS =             (101, "Switching Protocols")
    PROCESSING =                      (102, "Processing")

    OK =                              (200, "OK")
    CREATED =                         (201, "Created")
    ACCEPTED =                        (202, "Accepted")
    NON_AUTHORITATIVE_INFORMATION =   (203, "Non-Authoritative Information")
    NO_CONTENT =                      (204, "No Content")
    RESET_CONTENT =                   (205, "Reset Content")
    PARTIAL_CONTENT =                 (206, "Partial Content")
    MULTI_STATUS =                    (207, "Multi-Status")
    ALREADY_REPORTED =                (208, "Already Reported")
    IM_USED =                         (226, "IM Used")

    MULTIPLE_CHOICES =                (300, "Multiple Choices")
    MOVED_PERMANENTLY =               (301, "Moved Permanently")
    FOUND =                           (302, "Found")
    SEE_OTHER =                       (303, "See Other")
    NOT_MODIFIED =                    (304, "Not Modified")
    USE_PROXY =                       (305, "Use Proxy")
    TEMPORARY_REDIRECT =              (307, "Temporary Redirect")
    PERMANENT_REDIRECT =              (308, "Permanent Redirect")

    BAD_REQUEST =                     (400, "Bad Request")
    UNAUTHORIZED =                    (401, "Unauthorized")
    PAYMENT_REQUIRED =                (402, "Payment Required")
    FORBIDDEN =                       (403, "Forbidden")
    NOT_FOUND =                       (404, "Not Found")
    METHOD_NOT_ALLOWED =              (405, "Method Not Allowed")
    NOT_ACCEPTABLE =                  (406, "Not Acceptable")
    PROXY_AUTHENTICATION_REQUIRED =   (407, "Proxy Authentication Required")
    REQUEST_TIMEOUT =                 (408, "Request Timeout")
    CONFLICT =                        (409, "Conflict")
    GONE =                            (410, "Gone")
    LENGTH_REQUIRED =                 (411, "Length Required")
    PRECONDITION_FAILED =             (412, "Precondition Failed")
    REQUEST_ENTITY_TOO_LARGE =        (413, "Request Entity Too Large")
    REQUEST_URI_TOO_LONG =            (414, "Request-URI Too Long")
    UNSUPPORTED_MEDIA_TYPE =          (415, "Unsupported Media Type")
    REQUESTED_RANGE_NOT_SATISFIABLE = (416, "Requested Range Not Satisfiable")
    EXPECTATION_FAILED =              (417, "Expectation Failed")

    INTERNAL_SERVER_ERROR =           (500, "Internal Server Error")
    NOT_IMPLEMENTED =                 (501, "Not Implemented")
    BAD_GATEWAY =                     (502, "Bad Gateway")
    SERVICE_UNAVAILABLE =             (503, "Service Unavailable")
    GATEWAY_TIMEOUT =                 (504, "Gateway Timeout")
    HTTP_VERSION_NOT_SUPPORTED =      (505, "HTTP Version Not Supported")
    VARIANT_ALSO_NEGOTIATES =         (506, "Variant Also Negotiates")
    INSUFFICIENT_STORAGE =            (507, "Insufficient Storage")
    LOOP_DETECTED =                   (508, "Loop Detected")
    BANDWIDTH_LIMIT_EXCEEDED =        (509, "Bandwidth Limit Exceeded")
    NOT_EXTENDED =                    (510, "Not Extended")


type 

  HttpPayload* = object of RootObj
    version*: float
    headers*: StringTableRef
    rawBody*: string

  

  Response* = object of HttpPayload
    status*: Status

method setHeader*(p: HttpPayload, key, value: string) {.base.} =
  p.headers[key] = value

method hasHeader*(p: HttpPayload, key: string): bool {.base.} =
  p.headers.hasKey(key)

method getHeader*(p: HttpPayload, key: string): string {.base.} =
  if p.hasHeader(key): p.headers[key] else: nil

type HttpParseErr = object of Exception
  discard

############
# Request. #
############

type Request* = object of HttpPayload
  httpMethod*: Method
  path: string
  host: string
  port: uint16

proc setPath*(r: Request, path: string) =
  var path = path
  if path == nil or path.len() < 1 or path[0] != '/':
    path = "/" & path

proc newRequest*(path, host: string, httpMethod: Method = Method.GET, port: uint16 = 80): Request =
  result = Request(
    `httpMethod`: httpMethod,
    host: host,
    port: port,
    `path`: path,
    headers: newStringTable(modeCaseInsensitive) 
  )
  result.setPath(path)

proc toString*(r: Request): string =
  result = r.httpMethod.`$` & " " & r.path & " HTTP/1.1\r\n"
  if not r.headers.hasKey("Host"):
    var port = if r.port == 80: "" else: ":" & r.port.`$`
    r.headers["Host"] = r.host & port
  for key, val in r.headers:
    result &= key & ": " & val & "\r\n"

  # Empty newline.
  result &= "\r\n"


proc newHttpParseErr(msg: string): ref HttpParseErr =
  newException(HttpParseErr, msg)

proc parseHttResponse*(s: string): Response =
  if s == nil or s == "":
    raise newHttpParseErr("Empty payload.")

  var parser = newStrParser(s)

  result = Response()

  # Get HTTP protocol version.
  var version = parser.skip("HTTP/").parseFloat()
  if version == "":
    raise newHttpParseErr("Malformed HTTP header, no protocol version found: " & s.splitLines()[0])
  elif version == "1.0":
    result.version = 1.0
  elif version == "1.1":
    result.version = 1.1
  else:
    raise newHttpParseErr("Unknown HTTP protocol version: " & version)

  # Parse status code.
  var status = parser.skipWhitespace().parseInteger()
  if status == "":
    raise newHttpParseErr("Malformed HTTP header, no http status found: " & s.splitLines()[0])
  elif status.len() != 3:
    raise newHttpParseErr("Unknown HTTP status code: " & status)
  else:
    result.status = Status(parseInt(status))
  discard parser.skipToNextLine() 

  # Parse headers.
  result.headers = newStringTable(modeCaseInsensitive)
  if not parser.endReached():
    while true:
      var header = parser.parseKeyValuePair()
      if header.key == nil:
        break
      result.headers[header.key] = header.value
      discard parser.skipToNextLine()
  
  if not parser.endReached(): 
    if not (parser.getNext(2) == "\r\n"):
      raise newHttpParseErr("Malformed HTTP payload: expected empty line after header.")

    # Rest of the content is the body.
    result.rawBody = parser.skipToNextLine().getRemaining()

