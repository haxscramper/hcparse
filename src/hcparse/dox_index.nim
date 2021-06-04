import std/[options]
import hmisc/hasts/[xml_ast]
export options, xml_ast

import hmisc/algo/halgorithm

type
  DoxygenType* = object
    version*: string
    compound*: seq[CompoundType]

  CompoundType* = object
    refid*: string
    kind*: CompoundKind
    name*: string
    member*: seq[MemberType]

  MemberType* = object
    refid*: string
    kind*: MemberKind
    name*: string

  CompoundKind* = enum
    ckClass,                ## XSD enumeration: `class`
    ckStruct,               ## XSD enumeration: `struct`
    ckUnion,                ## XSD enumeration: `union`
    ckInterface,            ## XSD enumeration: `interface`
    ckProtocol,             ## XSD enumeration: `protocol`
    ckCategory,             ## XSD enumeration: `category`
    ckException,            ## XSD enumeration: `exception`
    ckFile,                 ## XSD enumeration: `file`
    ckNamespace,            ## XSD enumeration: `namespace`
    ckGroup,                ## XSD enumeration: `group`
    ckPage,                 ## XSD enumeration: `page`
    ckExample,              ## XSD enumeration: `example`
    ckDir,                  ## XSD enumeration: `dir`
    ckType                   ## XSD enumeration: `type`
  MemberKind* = enum
    mkDefine,               ## XSD enumeration: `define`
    mkProperty,             ## XSD enumeration: `property`
    mkEvent,                ## XSD enumeration: `event`
    mkVariable,             ## XSD enumeration: `variable`
    mkTypedef,              ## XSD enumeration: `typedef`
    mkEnum,                 ## XSD enumeration: `enum`
    mkEnumvalue,            ## XSD enumeration: `enumvalue`
    mkFunction,             ## XSD enumeration: `function`
    mkSignal,               ## XSD enumeration: `signal`
    mkPrototype,            ## XSD enumeration: `prototype`
    mkFriend,               ## XSD enumeration: `friend`
    mkDcop,                 ## XSD enumeration: `dcop`
    mkSlot                   ## XSD enumeration: `slot`

proc loadXml*(parser: var HXmlParser; target: var DoxygenType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var CompoundType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var MemberType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var CompoundKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var MemberKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxygenType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "version":
        loadXml(parser, target.version, "version")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "compound":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.compound, "compound")
      else:
        ## 712:4:xml_to_types.nim
        if inMixed:
          return
        else:
          raiseUnexpectedElement(parser, tag)
    of XmlEventKind.xmlElementClose:
      parser.next()
    of XmlEventKind.xmlElementEnd:
      if parser.elementName() == tag:
        parser.next()
        break
      else:
        raiseUnexpectedElement(parser, tag)
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlCharData,
        XmlEventKind.xmlWhitespace, XmlEventKind.xmlComment, XmlEventKind.xmlPI,
        XmlEventKind.xmlCData, XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var CompoundType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "kind":
        loadXml(parser, target.kind, "kind")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "name":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.name, "name")
      of "member":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.member, "member")
      else:
        ## 712:4:xml_to_types.nim
        if inMixed:
          return
        else:
          raiseUnexpectedElement(parser, tag)
    of XmlEventKind.xmlElementClose:
      parser.next()
    of XmlEventKind.xmlElementEnd:
      if parser.elementName() == tag:
        parser.next()
        break
      else:
        raiseUnexpectedElement(parser, tag)
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlCharData,
        XmlEventKind.xmlWhitespace, XmlEventKind.xmlComment, XmlEventKind.xmlPI,
        XmlEventKind.xmlCData, XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var MemberType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "kind":
        loadXml(parser, target.kind, "kind")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "name":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.name, "name")
      else:
        ## 712:4:xml_to_types.nim
        if inMixed:
          return
        else:
          raiseUnexpectedElement(parser, tag)
    of XmlEventKind.xmlElementClose:
      parser.next()
    of XmlEventKind.xmlElementEnd:
      if parser.elementName() == tag:
        parser.next()
        break
      else:
        raiseUnexpectedElement(parser, tag)
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlCharData,
        XmlEventKind.xmlWhitespace, XmlEventKind.xmlComment, XmlEventKind.xmlPI,
        XmlEventKind.xmlCData, XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var CompoundKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "class":
    target = ckClass
  of "struct":
    target = ckStruct
  of "union":
    target = ckUnion
  of "interface":
    target = ckInterface
  of "protocol":
    target = ckProtocol
  of "category":
    target = ckCategory
  of "exception":
    target = ckException
  of "file":
    target = ckFile
  of "namespace":
    target = ckNamespace
  of "group":
    target = ckGroup
  of "page":
    target = ckPage
  of "example":
    target = ckExample
  of "dir":
    target = ckDir
  of "type":
    target = ckType
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var MemberKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "define":
    target = mkDefine
  of "property":
    target = mkProperty
  of "event":
    target = mkEvent
  of "variable":
    target = mkVariable
  of "typedef":
    target = mkTypedef
  of "enum":
    target = mkEnum
  of "enumvalue":
    target = mkEnumvalue
  of "function":
    target = mkFunction
  of "signal":
    target = mkSignal
  of "prototype":
    target = mkPrototype
  of "friend":
    target = mkFriend
  of "dcop":
    target = mkDcop
  of "slot":
    target = mkSlot
  parser.next()
