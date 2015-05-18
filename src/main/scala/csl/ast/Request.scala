package csl.ast

case class Variable(name: String, req: Request, res: Response)

case class Request(o: Object)
case class Response(o: Object)

case class Property(key: String, value: Value)

