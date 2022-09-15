import std/[options]
import hmisc/hasts/[xml_ast]
export options, xml_ast

import hmisc/algo/halgorithm

type
  DoxygenType* = object
    version*: DoxVersionNumber
    compounddef*: seq[CompounddefType]

  CompounddefType* = object
    id*: string
    kind*: DoxCompoundKind
    language*: Option[DoxLanguage]
    prot*: DoxProtectionKind
    final*: Option[DoxBool]
    inline*: Option[DoxBool]
    sealed*: Option[DoxBool]
    abstract*: Option[DoxBool]
    compoundname*: string
    title*: Option[string]
    basecompoundref*: seq[CompoundRefType]
    derivedcompoundref*: seq[CompoundRefType]
    includes*: seq[IncType]
    includedby*: seq[IncType]
    incdepgraph*: Option[GraphType]
    invincdepgraph*: Option[GraphType]
    innerdir*: seq[RefType]
    innerfile*: seq[RefType]
    innerclass*: seq[RefType]
    innernamespace*: seq[RefType]
    innerpage*: seq[RefType]
    innergroup*: seq[RefType]
    templateparamlist*: Option[TemplateparamlistType]
    sectiondef*: seq[SectiondefType]
    tableofcontents*: Option[TableofcontentsType]
    briefdescription*: Option[DescriptionType]
    detaileddescription*: Option[DescriptionType]
    inheritancegraph*: Option[GraphType]
    collaborationgraph*: Option[GraphType]
    programlisting*: Option[ListingType]
    location*: Option[LocationType]
    listofallmembers*: Option[ListofallmembersType]

  ListofallmembersType* = object
    member*: seq[MemberRefType]

  MemberRefType* = object
    refid*: string
    prot*: DoxProtectionKind
    virt*: DoxVirtualKind
    ambiguityscope*: string
    scope*: XmlNode
    name*: XmlNode

  DocHtmlOnlyType* = object
    fBlock*: string
    baseExt*: string

  CompoundRefType* = object
    refid*: Option[string]
    prot*: DoxProtectionKind
    virt*: DoxVirtualKind
    baseExt*: string

  ReimplementType* = object
    refid*: string
    baseExt*: string

  IncType* = object
    refid*: string
    local*: DoxBool
    baseExt*: string

  RefType* = object
    refid*: string
    prot*: Option[DoxProtectionKind]
    inline*: Option[DoxBool]
    baseExt*: string

  RefTextType* = object
    refid*: string
    kindref*: DoxRefKind
    external*: Option[string]
    tooltip*: Option[string]
    baseExt*: string

  SectiondefType* = object
    kind*: DoxSectionKind
    header*: Option[string]
    description*: Option[DescriptionType]
    memberdef*: seq[MemberdefType]

  MemberdefType* = object
    kind*: DoxMemberKind
    id*: string
    prot*: DoxProtectionKind
    fStatic*: DoxBool
    strong*: Option[DoxBool]
    fConst*: Option[DoxBool]
    explicit*: Option[DoxBool]
    inline*: Option[DoxBool]
    refqual*: Option[DoxRefQualifierKind]
    virt*: Option[DoxVirtualKind]
    volatile*: Option[DoxBool]
    mutable*: Option[DoxBool]
    noexcept*: Option[DoxBool]
    constexpr*: Option[DoxBool]
    readable*: Option[DoxBool]
    writable*: Option[DoxBool]
    initonly*: Option[DoxBool]
    settable*: Option[DoxBool]
    privatesettable*: Option[DoxBool]
    protectedsettable*: Option[DoxBool]
    gettable*: Option[DoxBool]
    privategettable*: Option[DoxBool]
    protectedgettable*: Option[DoxBool]
    final*: Option[DoxBool]
    sealed*: Option[DoxBool]
    new*: Option[DoxBool]
    add*: Option[DoxBool]
    remove*: Option[DoxBool]
    fRaise*: Option[DoxBool]
    optional*: Option[DoxBool]
    required*: Option[DoxBool]
    accessor*: Option[DoxAccessor]
    attribute*: Option[DoxBool]
    property*: Option[DoxBool]
    readonly*: Option[DoxBool]
    bound*: Option[DoxBool]
    removable*: Option[DoxBool]
    constrained*: Option[DoxBool]
    transient*: Option[DoxBool]
    maybevoid*: Option[DoxBool]
    maybedefault*: Option[DoxBool]
    maybeambiguous*: Option[DoxBool]
    templateparamlist*: Option[TemplateparamlistType]
    fType*: Option[LinkedTextType]
    definition*: Option[XmlNode]
    argsstring*: Option[XmlNode]
    name*: XmlNode
    read*: Option[XmlNode]
    write*: Option[XmlNode]
    bitfield*: Option[XmlNode]
    reimplements*: seq[ReimplementType]
    reimplementedby*: seq[ReimplementType]
    param*: seq[ParamType]
    enumvalue*: seq[EnumvalueType]
    initializer*: Option[LinkedTextType]
    exceptions*: Option[LinkedTextType]
    briefdescription*: Option[DescriptionType]
    detaileddescription*: Option[DescriptionType]
    inbodydescription*: Option[DescriptionType]
    location*: LocationType
    references*: seq[ReferenceType]
    referencedby*: seq[ReferenceType]

  DescriptionTypeKind* = enum
    dtTitle, dtPara, dtInternal, dtSect1, dtMixedStr
  DescriptionTypeBody* = object
    case kind*: DescriptionTypeKind
    of dtTitle:
        fString*: string

    of dtPara:
        docParaType*: DocParaType

    of dtInternal:
        docInternalType*: DocInternalType

    of dtSect1:
        docSect1Type*: DocSect1Type

    of dtMixedStr:
        mixedStr*: string

  
  DescriptionType* = object
    xsdChoice*: seq[DescriptionTypeBody]

  EnumvalueTypeKind* = enum
    etName, etInitializer, etBriefdescription, etDetaileddescription, etMixedStr
  EnumvalueTypeBody* = object
    case kind*: EnumvalueTypeKind
    of etName:
        xmlNode*: XmlNode

    of etInitializer:
        linkedTextType*: LinkedTextType

    of etBriefdescription, etDetaileddescription:
        descriptionType*: DescriptionType

    of etMixedStr:
        mixedStr*: string

  
  EnumvalueType* = object
    id*: string
    prot*: DoxProtectionKind
    xsdChoice*: seq[EnumvalueTypeBody]

  TemplateparamlistType* = object
    param*: seq[ParamType]

  ParamType* = object
    attributes*: Option[XmlNode]
    fType*: Option[LinkedTextType]
    declname*: Option[XmlNode]
    defname*: Option[XmlNode]
    array*: Option[XmlNode]
    defval*: Option[LinkedTextType]
    typeconstraint*: Option[LinkedTextType]
    briefdescription*: Option[DescriptionType]

  LinkedTextTypeKind* = enum
    lttRef, lttMixedStr
  LinkedTextTypeBody* = object
    case kind*: LinkedTextTypeKind
    of lttRef:
        refTextType*: RefTextType

    of lttMixedStr:
        mixedStr*: string

  
  LinkedTextType* = object
    xsdChoice*: seq[LinkedTextTypeBody]

  GraphType* = object
    node*: seq[NodeType]

  NodeType* = object
    id*: string
    label*: XmlNode
    link*: Option[LinkType]
    childnode*: seq[ChildnodeType]

  ChildnodeType* = object
    refid*: string
    relation*: DoxGraphRelation
    edgelabel*: seq[XmlNode]

  LinkType* = object
    refid*: string
    external*: Option[string]

  ListingType* = object
    filename*: Option[string]
    codeline*: seq[CodelineType]

  CodelineType* = object
    lineno*: int
    refid*: string
    refkind*: DoxRefKind
    external*: DoxBool
    highlight*: seq[HighlightType]

  HighlightTypeKind* = enum
    htSp, htRef, htMixedStr
  HighlightTypeBody* = object
    case kind*: HighlightTypeKind
    of htSp:
        spType*: SpType

    of htRef:
        refTextType*: RefTextType

    of htMixedStr:
        mixedStr*: string

  
  HighlightType* = object
    class*: DoxHighlightClass
    xsdChoice*: seq[HighlightTypeBody]

  SpTypeKind* = enum
    stMixedStr
  SpTypeBody* = object
    case kind*: SpTypeKind
    of stMixedStr:
        mixedStr*: string

  
  SpType* = object
    value*: Option[int]
    xsdChoice*: seq[SpTypeBody]

  ReferenceTypeKind* = enum
    rtMixedStr
  ReferenceTypeBody* = object
    case kind*: ReferenceTypeKind
    of rtMixedStr:
        mixedStr*: string

  
  ReferenceType* = object
    refid*: string
    compoundref*: Option[string]
    startline*: int
    endline*: int
    xsdChoice*: seq[ReferenceTypeBody]

  LocationType* = object
    file*: string
    line*: int
    column*: Option[int]
    declfile*: Option[string]
    declline*: Option[int]
    declcolumn*: Option[int]
    bodyfile*: string
    bodystart*: int
    bodyend*: int

  DocSect1TypeKind* = enum
    dstTitle, dstMixedStr
  DocSect1TypeBody* = object
    case kind*: DocSect1TypeKind
    of dstTitle:
        fString*: string

    of dstMixedStr:
        mixedStr*: string

  
  DocSect1Type* = object
    id*: string
    xsdChoice*: seq[DocSect1TypeBody]

  DocSect2TypeKind* = enum
    dostTitle, dostMixedStr
  DocSect2TypeBody* = object
    case kind*: DocSect2TypeKind
    of dostTitle:
        fString*: string

    of dostMixedStr:
        mixedStr*: string

  
  DocSect2Type* = object
    id*: string
    xsdChoice*: seq[DocSect2TypeBody]

  DocSect3TypeKind* = enum
    docstTitle, docstMixedStr
  DocSect3TypeBody* = object
    case kind*: DocSect3TypeKind
    of docstTitle:
        fString*: string

    of docstMixedStr:
        mixedStr*: string

  
  DocSect3Type* = object
    id*: string
    xsdChoice*: seq[DocSect3TypeBody]

  DocSect4TypeKind* = enum
    docsetTitle, docsetMixedStr
  DocSect4TypeBody* = object
    case kind*: DocSect4TypeKind
    of docsetTitle:
        fString*: string

    of docsetMixedStr:
        mixedStr*: string

  
  DocSect4Type* = object
    id*: string
    xsdChoice*: seq[DocSect4TypeBody]

  DocInternalTypeKind* = enum
    ditPara, ditSect1, ditMixedStr
  DocInternalTypeBody* = object
    case kind*: DocInternalTypeKind
    of ditPara:
        docParaType*: DocParaType

    of ditSect1:
        docSect1Type*: DocSect1Type

    of ditMixedStr:
        mixedStr*: string

  
  DocInternalType* = object
    xsdChoice*: seq[DocInternalTypeBody]

  DocInternalS1TypeKind* = enum
    distPara, distSect2, distMixedStr
  DocInternalS1TypeBody* = object
    case kind*: DocInternalS1TypeKind
    of distPara:
        docParaType*: DocParaType

    of distSect2:
        docSect2Type*: DocSect2Type

    of distMixedStr:
        mixedStr*: string

  
  DocInternalS1Type* = object
    xsdChoice*: seq[DocInternalS1TypeBody]

  DocInternalS2TypeKind* = enum
    dis2tPara, dis2tSect3, dis2tMixedStr
  DocInternalS2TypeBody* = object
    case kind*: DocInternalS2TypeKind
    of dis2tPara:
        docParaType*: DocParaType

    of dis2tSect3:
        docSect3Type*: DocSect3Type

    of dis2tMixedStr:
        mixedStr*: string

  
  DocInternalS2Type* = object
    xsdChoice*: seq[DocInternalS2TypeBody]

  DocInternalS3TypeKind* = enum
    dis3tPara, dis3tSect3, dis3tMixedStr
  DocInternalS3TypeBody* = object
    case kind*: DocInternalS3TypeKind
    of dis3tPara:
        docParaType*: DocParaType

    of dis3tSect3:
        docSect4Type*: DocSect4Type

    of dis3tMixedStr:
        mixedStr*: string

  
  DocInternalS3Type* = object
    xsdChoice*: seq[DocInternalS3TypeBody]

  DocInternalS4TypeKind* = enum
    dis4tPara, dis4tMixedStr
  DocInternalS4TypeBody* = object
    case kind*: DocInternalS4TypeKind
    of dis4tPara:
        docParaType*: DocParaType

    of dis4tMixedStr:
        mixedStr*: string

  
  DocInternalS4Type* = object
    xsdChoice*: seq[DocInternalS4TypeBody]

  DocTitleTypeKind* = enum
    dttUlink, dttBold, dttS, dttStrike, dttUnderline, dttEmphasis,
    dttComputeroutput, dttSubscript, dttSuperscript, dttCenter, dttSmall,
    dttDel, dttIns, dttHtmlonly, dttManonly, dttXmlonly, dttRtfonly,
    dttLatexonly, dttDocbookonly, dttImage, dttDot, dttMsc, dttPlantuml,
    dttAnchor, dttFormula, dttRef, dttEmoji, dttLinebreak, dttNonbreakablespace,
    dttIexcl, dttCent, dttPound, dttCurren, dttYen, dttBrvbar, dttSect,
    dttUmlaut, dttCopy, dttOrdf, dttLaquo, dttNot, dttShy, dttRegistered,
    dttMacr, dttDeg, dttPlusmn, dttSup2, dttSup3, dttAcute, dttMicro, dttPara,
    dttMiddot, dttCedil, dttSup1, dttOrdm, dttRaquo, dttFrac14, dttFrac12,
    dttFrac34, dttIquest, dttAgrave, dttAacute, dttAcirc, dttAtilde, dttAumlaut,
    dttAring, dttAElig, dttCcedil, dttEgrave, dttEacute, dttEcirc, dttEumlaut,
    dttIgrave, dttIacute, dttIcirc, dttIumlaut, dttETH, dttNtilde, dttOgrave,
    dttOacute, dttOcirc, dttOtilde, dttOumlaut, dttTimes, dttOslash, dttUgrave,
    dttUacute, dttUcirc, dttUumlaut, dttYacute, dttTHORN, dttSzlig, dttAgrave1,
    dttAacute1, dttAcirc1, dttAtilde1, dttAumlaut1, dttAring1, dttAelig1,
    dttCcedil1, dttEgrave1, dttEacute1, dttEcirc1, dttEumlaut1, dttIgrave1,
    dttIacute1, dttIcirc1, dttIumlaut1, dttEth1, dttNtilde1, dttOgrave1,
    dttOacute1, dttOcirc1, dttOtilde1, dttOumlaut1, dttDivide, dttOslash1,
    dttUgrave1, dttUacute1, dttUcirc1, dttUumlaut1, dttYacute1, dttThorn1,
    dttYumlaut, dttFnof, dttAlpha, dttBeta, dttGamma, dttDelta, dttEpsilon,
    dttZeta, dttEta, dttTheta, dttIota, dttKappa, dttLambda, dttMu, dttNu,
    dttXi, dttOmicron, dttPi, dttRho, dttSigma, dttTau, dttUpsilon, dttPhi,
    dttChi, dttPsi, dttOmega, dttAlpha1, dttBeta1, dttGamma1, dttDelta1,
    dttEpsilon1, dttZeta1, dttEta1, dttTheta1, dttIota1, dttKappa1, dttLambda1,
    dttMu1, dttNu1, dttXi1, dttOmicron1, dttPi1, dttRho1, dttSigmaf, dttSigma1,
    dttTau1, dttUpsilon1, dttPhi1, dttChi1, dttPsi1, dttOmega1, dttThetasym,
    dttUpsih, dttPiv, dttBull, dttHellip, dttPrime, dttPrime1, dttOline,
    dttFrasl, dttWeierp, dttImaginary, dttReal, dttTrademark, dttAlefsym,
    dttLarr, dttUarr, dttRarr, dttDarr, dttHarr, dttCrarr, dttLArr1, dttUArr1,
    dttRArr1, dttDArr1, dttHArr1, dttForall, dttPart, dttExist, dttEmpty,
    dttNabla, dttIsin, dttNotin, dttNi, dttProd, dttSum, dttMinus, dttLowast,
    dttRadic, dttProp, dttInfin, dttAng, dttAnd, dttOr, dttCap, dttCup, dttInt,
    dttThere4, dttSim, dttCong, dttAsymp, dttNe, dttEquiv, dttLe, dttGe, dttSub,
    dttSup, dttNsub, dttSube, dttSupe, dttOplus, dttOtimes, dttPerp, dttSdot,
    dttLceil, dttRceil, dttLfloor, dttRfloor, dttLang, dttRang, dttLoz,
    dttSpades, dttClubs, dttHearts, dttDiams, dttOElig, dttOelig1, dttScaron,
    dttScaron1, dttYumlaut1, dttCirc, dttTilde, dttEnsp, dttEmsp, dttThinsp,
    dttZwnj, dttZwj, dttLrm, dttRlm, dttNdash, dttMdash, dttLsquo, dttRsquo,
    dttSbquo, dttLdquo, dttRdquo, dttBdquo, dttDagger, dttDagger1, dttPermil,
    dttLsaquo, dttRsaquo, dttEuro, dttTm, dttMixedStr
  DocTitleTypeBody* = object
    case kind*: DocTitleTypeKind
    of dttUlink:
        docURLLink*: DocURLLink

    of dttBold, dttS, dttStrike, dttUnderline, dttEmphasis, dttComputeroutput,
       dttSubscript, dttSuperscript, dttCenter, dttSmall, dttDel, dttIns:
        docMarkupType*: DocMarkupType

    of dttHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of dttManonly, dttXmlonly, dttRtfonly, dttLatexonly, dttDocbookonly:
        fString*: string

    of dttImage, dttDot, dttMsc, dttPlantuml:
        docImageType*: DocImageType

    of dttAnchor:
        docAnchorType*: DocAnchorType

    of dttFormula:
        docFormulaType*: DocFormulaType

    of dttRef:
        docRefTextType*: DocRefTextType

    of dttEmoji:
        docEmojiType*: DocEmojiType

    of dttLinebreak, dttNonbreakablespace, dttIexcl, dttCent, dttPound,
       dttCurren, dttYen, dttBrvbar, dttSect, dttUmlaut, dttCopy, dttOrdf,
       dttLaquo, dttNot, dttShy, dttRegistered, dttMacr, dttDeg, dttPlusmn,
       dttSup2, dttSup3, dttAcute, dttMicro, dttPara, dttMiddot, dttCedil,
       dttSup1, dttOrdm, dttRaquo, dttFrac14, dttFrac12, dttFrac34, dttIquest,
       dttAgrave, dttAacute, dttAcirc, dttAtilde, dttAumlaut, dttAring,
       dttAElig, dttCcedil, dttEgrave, dttEacute, dttEcirc, dttEumlaut,
       dttIgrave, dttIacute, dttIcirc, dttIumlaut, dttETH, dttNtilde, dttOgrave,
       dttOacute, dttOcirc, dttOtilde, dttOumlaut, dttTimes, dttOslash,
       dttUgrave, dttUacute, dttUcirc, dttUumlaut, dttYacute, dttTHORN,
       dttSzlig, dttAgrave1, dttAacute1, dttAcirc1, dttAtilde1, dttAumlaut1,
       dttAring1, dttAelig1, dttCcedil1, dttEgrave1, dttEacute1, dttEcirc1,
       dttEumlaut1, dttIgrave1, dttIacute1, dttIcirc1, dttIumlaut1, dttEth1,
       dttNtilde1, dttOgrave1, dttOacute1, dttOcirc1, dttOtilde1, dttOumlaut1,
       dttDivide, dttOslash1, dttUgrave1, dttUacute1, dttUcirc1, dttUumlaut1,
       dttYacute1, dttThorn1, dttYumlaut, dttFnof, dttAlpha, dttBeta, dttGamma,
       dttDelta, dttEpsilon, dttZeta, dttEta, dttTheta, dttIota, dttKappa,
       dttLambda, dttMu, dttNu, dttXi, dttOmicron, dttPi, dttRho, dttSigma,
       dttTau, dttUpsilon, dttPhi, dttChi, dttPsi, dttOmega, dttAlpha1,
       dttBeta1, dttGamma1, dttDelta1, dttEpsilon1, dttZeta1, dttEta1,
       dttTheta1, dttIota1, dttKappa1, dttLambda1, dttMu1, dttNu1, dttXi1,
       dttOmicron1, dttPi1, dttRho1, dttSigmaf, dttSigma1, dttTau1, dttUpsilon1,
       dttPhi1, dttChi1, dttPsi1, dttOmega1, dttThetasym, dttUpsih, dttPiv,
       dttBull, dttHellip, dttPrime, dttPrime1, dttOline, dttFrasl, dttWeierp,
       dttImaginary, dttReal, dttTrademark, dttAlefsym, dttLarr, dttUarr,
       dttRarr, dttDarr, dttHarr, dttCrarr, dttLArr1, dttUArr1, dttRArr1,
       dttDArr1, dttHArr1, dttForall, dttPart, dttExist, dttEmpty, dttNabla,
       dttIsin, dttNotin, dttNi, dttProd, dttSum, dttMinus, dttLowast, dttRadic,
       dttProp, dttInfin, dttAng, dttAnd, dttOr, dttCap, dttCup, dttInt,
       dttThere4, dttSim, dttCong, dttAsymp, dttNe, dttEquiv, dttLe, dttGe,
       dttSub, dttSup, dttNsub, dttSube, dttSupe, dttOplus, dttOtimes, dttPerp,
       dttSdot, dttLceil, dttRceil, dttLfloor, dttRfloor, dttLang, dttRang,
       dttLoz, dttSpades, dttClubs, dttHearts, dttDiams, dttOElig, dttOelig1,
       dttScaron, dttScaron1, dttYumlaut1, dttCirc, dttTilde, dttEnsp, dttEmsp,
       dttThinsp, dttZwnj, dttZwj, dttLrm, dttRlm, dttNdash, dttMdash, dttLsquo,
       dttRsquo, dttSbquo, dttLdquo, dttRdquo, dttBdquo, dttDagger, dttDagger1,
       dttPermil, dttLsaquo, dttRsaquo, dttEuro, dttTm:
        docEmptyType*: DocEmptyType

    of dttMixedStr:
        mixedStr*: string

  
  DocTitleType* = object
    xsdChoice*: seq[DocTitleTypeBody]

  DocParaTypeKind* = enum
    dptUlink, dptBold, dptS, dptStrike, dptUnderline, dptEmphasis,
    dptComputeroutput, dptSubscript, dptSuperscript, dptCenter, dptSmall,
    dptDel, dptIns, dptPreformatted, dptHtmlonly, dptManonly, dptXmlonly,
    dptRtfonly, dptLatexonly, dptDocbookonly, dptVerbatim, dptImage, dptDot,
    dptMsc, dptPlantuml, dptDotfile, dptMscfile, dptDiafile, dptAnchor,
    dptFormula, dptRef, dptEmoji, dptLinebreak, dptNonbreakablespace, dptIexcl,
    dptCent, dptPound, dptCurren, dptYen, dptBrvbar, dptSect, dptUmlaut,
    dptCopy, dptOrdf, dptLaquo, dptNot, dptShy, dptRegistered, dptMacr, dptDeg,
    dptPlusmn, dptSup2, dptSup3, dptAcute, dptMicro, dptPara, dptMiddot,
    dptCedil, dptSup1, dptOrdm, dptRaquo, dptFrac14, dptFrac12, dptFrac34,
    dptIquest, dptAgrave, dptAacute, dptAcirc, dptAtilde, dptAumlaut, dptAring,
    dptAElig, dptCcedil, dptEgrave, dptEacute, dptEcirc, dptEumlaut, dptIgrave,
    dptIacute, dptIcirc, dptIumlaut, dptETH, dptNtilde, dptOgrave, dptOacute,
    dptOcirc, dptOtilde, dptOumlaut, dptTimes, dptOslash, dptUgrave, dptUacute,
    dptUcirc, dptUumlaut, dptYacute, dptTHORN, dptSzlig, dptAgrave1, dptAacute1,
    dptAcirc1, dptAtilde1, dptAumlaut1, dptAring1, dptAelig1, dptCcedil1,
    dptEgrave1, dptEacute1, dptEcirc1, dptEumlaut1, dptIgrave1, dptIacute1,
    dptIcirc1, dptIumlaut1, dptEth1, dptNtilde1, dptOgrave1, dptOacute1,
    dptOcirc1, dptOtilde1, dptOumlaut1, dptDivide, dptOslash1, dptUgrave1,
    dptUacute1, dptUcirc1, dptUumlaut1, dptYacute1, dptThorn1, dptYumlaut,
    dptFnof, dptAlpha, dptBeta, dptGamma, dptDelta, dptEpsilon, dptZeta, dptEta,
    dptTheta, dptIota, dptKappa, dptLambda, dptMu, dptNu, dptXi, dptOmicron,
    dptPi, dptRho, dptSigma, dptTau, dptUpsilon, dptPhi, dptChi, dptPsi,
    dptOmega, dptAlpha1, dptBeta1, dptGamma1, dptDelta1, dptEpsilon1, dptZeta1,
    dptEta1, dptTheta1, dptIota1, dptKappa1, dptLambda1, dptMu1, dptNu1, dptXi1,
    dptOmicron1, dptPi1, dptRho1, dptSigmaf, dptSigma1, dptTau1, dptUpsilon1,
    dptPhi1, dptChi1, dptPsi1, dptOmega1, dptThetasym, dptUpsih, dptPiv,
    dptBull, dptHellip, dptPrime, dptPrime1, dptOline, dptFrasl, dptWeierp,
    dptImaginary, dptReal, dptTrademark, dptAlefsym, dptLarr, dptUarr, dptRarr,
    dptDarr, dptHarr, dptCrarr, dptLArr1, dptUArr1, dptRArr1, dptDArr1,
    dptHArr1, dptForall, dptPart, dptExist, dptEmpty, dptNabla, dptIsin,
    dptNotin, dptNi, dptProd, dptSum, dptMinus, dptLowast, dptRadic, dptProp,
    dptInfin, dptAng, dptAnd, dptOr, dptCap, dptCup, dptInt, dptThere4, dptSim,
    dptCong, dptAsymp, dptNe, dptEquiv, dptLe, dptGe, dptSub, dptSup, dptNsub,
    dptSube, dptSupe, dptOplus, dptOtimes, dptPerp, dptSdot, dptLceil, dptRceil,
    dptLfloor, dptRfloor, dptLang, dptRang, dptLoz, dptSpades, dptClubs,
    dptHearts, dptDiams, dptOElig, dptOelig1, dptScaron, dptScaron1,
    dptYumlaut1, dptCirc, dptTilde, dptEnsp, dptEmsp, dptThinsp, dptZwnj,
    dptZwj, dptLrm, dptRlm, dptNdash, dptMdash, dptLsquo, dptRsquo, dptSbquo,
    dptLdquo, dptRdquo, dptBdquo, dptDagger, dptDagger1, dptPermil, dptLsaquo,
    dptRsaquo, dptEuro, dptTm, dptHruler, dptProgramlisting, dptIndexentry,
    dptOrderedlist, dptItemizedlist, dptSimplesect, dptTitle, dptVariablelist,
    dptTable, dptHeading, dptToclist, dptLanguage, dptParameterlist,
    dptXrefsect, dptCopydoc, dptBlockquote, dptParblock, dptMixedStr
  DocParaTypeBody* = object
    case kind*: DocParaTypeKind
    of dptUlink:
        docURLLink*: DocURLLink

    of dptBold, dptS, dptStrike, dptUnderline, dptEmphasis, dptComputeroutput,
       dptSubscript, dptSuperscript, dptCenter, dptSmall, dptDel, dptIns,
       dptPreformatted:
        docMarkupType*: DocMarkupType

    of dptHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of dptManonly, dptXmlonly, dptRtfonly, dptLatexonly, dptDocbookonly,
       dptVerbatim:
        fString*: string

    of dptImage, dptDot, dptMsc, dptPlantuml, dptDotfile, dptMscfile, dptDiafile:
        docImageType*: DocImageType

    of dptAnchor:
        docAnchorType*: DocAnchorType

    of dptFormula:
        docFormulaType*: DocFormulaType

    of dptRef:
        docRefTextType*: DocRefTextType

    of dptEmoji:
        docEmojiType*: DocEmojiType

    of dptLinebreak, dptNonbreakablespace, dptIexcl, dptCent, dptPound,
       dptCurren, dptYen, dptBrvbar, dptSect, dptUmlaut, dptCopy, dptOrdf,
       dptLaquo, dptNot, dptShy, dptRegistered, dptMacr, dptDeg, dptPlusmn,
       dptSup2, dptSup3, dptAcute, dptMicro, dptPara, dptMiddot, dptCedil,
       dptSup1, dptOrdm, dptRaquo, dptFrac14, dptFrac12, dptFrac34, dptIquest,
       dptAgrave, dptAacute, dptAcirc, dptAtilde, dptAumlaut, dptAring,
       dptAElig, dptCcedil, dptEgrave, dptEacute, dptEcirc, dptEumlaut,
       dptIgrave, dptIacute, dptIcirc, dptIumlaut, dptETH, dptNtilde, dptOgrave,
       dptOacute, dptOcirc, dptOtilde, dptOumlaut, dptTimes, dptOslash,
       dptUgrave, dptUacute, dptUcirc, dptUumlaut, dptYacute, dptTHORN,
       dptSzlig, dptAgrave1, dptAacute1, dptAcirc1, dptAtilde1, dptAumlaut1,
       dptAring1, dptAelig1, dptCcedil1, dptEgrave1, dptEacute1, dptEcirc1,
       dptEumlaut1, dptIgrave1, dptIacute1, dptIcirc1, dptIumlaut1, dptEth1,
       dptNtilde1, dptOgrave1, dptOacute1, dptOcirc1, dptOtilde1, dptOumlaut1,
       dptDivide, dptOslash1, dptUgrave1, dptUacute1, dptUcirc1, dptUumlaut1,
       dptYacute1, dptThorn1, dptYumlaut, dptFnof, dptAlpha, dptBeta, dptGamma,
       dptDelta, dptEpsilon, dptZeta, dptEta, dptTheta, dptIota, dptKappa,
       dptLambda, dptMu, dptNu, dptXi, dptOmicron, dptPi, dptRho, dptSigma,
       dptTau, dptUpsilon, dptPhi, dptChi, dptPsi, dptOmega, dptAlpha1,
       dptBeta1, dptGamma1, dptDelta1, dptEpsilon1, dptZeta1, dptEta1,
       dptTheta1, dptIota1, dptKappa1, dptLambda1, dptMu1, dptNu1, dptXi1,
       dptOmicron1, dptPi1, dptRho1, dptSigmaf, dptSigma1, dptTau1, dptUpsilon1,
       dptPhi1, dptChi1, dptPsi1, dptOmega1, dptThetasym, dptUpsih, dptPiv,
       dptBull, dptHellip, dptPrime, dptPrime1, dptOline, dptFrasl, dptWeierp,
       dptImaginary, dptReal, dptTrademark, dptAlefsym, dptLarr, dptUarr,
       dptRarr, dptDarr, dptHarr, dptCrarr, dptLArr1, dptUArr1, dptRArr1,
       dptDArr1, dptHArr1, dptForall, dptPart, dptExist, dptEmpty, dptNabla,
       dptIsin, dptNotin, dptNi, dptProd, dptSum, dptMinus, dptLowast, dptRadic,
       dptProp, dptInfin, dptAng, dptAnd, dptOr, dptCap, dptCup, dptInt,
       dptThere4, dptSim, dptCong, dptAsymp, dptNe, dptEquiv, dptLe, dptGe,
       dptSub, dptSup, dptNsub, dptSube, dptSupe, dptOplus, dptOtimes, dptPerp,
       dptSdot, dptLceil, dptRceil, dptLfloor, dptRfloor, dptLang, dptRang,
       dptLoz, dptSpades, dptClubs, dptHearts, dptDiams, dptOElig, dptOelig1,
       dptScaron, dptScaron1, dptYumlaut1, dptCirc, dptTilde, dptEnsp, dptEmsp,
       dptThinsp, dptZwnj, dptZwj, dptLrm, dptRlm, dptNdash, dptMdash, dptLsquo,
       dptRsquo, dptSbquo, dptLdquo, dptRdquo, dptBdquo, dptDagger, dptDagger1,
       dptPermil, dptLsaquo, dptRsaquo, dptEuro, dptTm, dptHruler:
        docEmptyType*: DocEmptyType

    of dptProgramlisting:
        listingType*: ListingType

    of dptIndexentry:
        docIndexEntryType*: DocIndexEntryType

    of dptOrderedlist, dptItemizedlist:
        docListType*: DocListType

    of dptSimplesect:
        docSimpleSectType*: DocSimpleSectType

    of dptTitle:
        docTitleType*: DocTitleType

    of dptVariablelist:
        docVariableListType*: DocVariableListType

    of dptTable:
        docTableType*: DocTableType

    of dptHeading:
        docHeadingType*: DocHeadingType

    of dptToclist:
        docTocListType*: DocTocListType

    of dptLanguage:
        docLanguageType*: DocLanguageType

    of dptParameterlist:
        docParamListType*: DocParamListType

    of dptXrefsect:
        docXRefSectType*: DocXRefSectType

    of dptCopydoc:
        docCopyType*: DocCopyType

    of dptBlockquote:
        docBlockQuoteType*: DocBlockQuoteType

    of dptParblock:
        docParBlockType*: DocParBlockType

    of dptMixedStr:
        mixedStr*: string

  
  DocParaType* = object
    xsdChoice*: seq[DocParaTypeBody]

  DocMarkupTypeKind* = enum
    dmtUlink, dmtBold, dmtS, dmtStrike, dmtUnderline, dmtEmphasis,
    dmtComputeroutput, dmtSubscript, dmtSuperscript, dmtCenter, dmtSmall,
    dmtDel, dmtIns, dmtPreformatted, dmtHtmlonly, dmtManonly, dmtXmlonly,
    dmtRtfonly, dmtLatexonly, dmtDocbookonly, dmtVerbatim, dmtImage, dmtDot,
    dmtMsc, dmtPlantuml, dmtDotfile, dmtMscfile, dmtDiafile, dmtAnchor,
    dmtFormula, dmtRef, dmtEmoji, dmtLinebreak, dmtNonbreakablespace, dmtIexcl,
    dmtCent, dmtPound, dmtCurren, dmtYen, dmtBrvbar, dmtSect, dmtUmlaut,
    dmtCopy, dmtOrdf, dmtLaquo, dmtNot, dmtShy, dmtRegistered, dmtMacr, dmtDeg,
    dmtPlusmn, dmtSup2, dmtSup3, dmtAcute, dmtMicro, dmtPara, dmtMiddot,
    dmtCedil, dmtSup1, dmtOrdm, dmtRaquo, dmtFrac14, dmtFrac12, dmtFrac34,
    dmtIquest, dmtAgrave, dmtAacute, dmtAcirc, dmtAtilde, dmtAumlaut, dmtAring,
    dmtAElig, dmtCcedil, dmtEgrave, dmtEacute, dmtEcirc, dmtEumlaut, dmtIgrave,
    dmtIacute, dmtIcirc, dmtIumlaut, dmtETH, dmtNtilde, dmtOgrave, dmtOacute,
    dmtOcirc, dmtOtilde, dmtOumlaut, dmtTimes, dmtOslash, dmtUgrave, dmtUacute,
    dmtUcirc, dmtUumlaut, dmtYacute, dmtTHORN, dmtSzlig, dmtAgrave1, dmtAacute1,
    dmtAcirc1, dmtAtilde1, dmtAumlaut1, dmtAring1, dmtAelig1, dmtCcedil1,
    dmtEgrave1, dmtEacute1, dmtEcirc1, dmtEumlaut1, dmtIgrave1, dmtIacute1,
    dmtIcirc1, dmtIumlaut1, dmtEth1, dmtNtilde1, dmtOgrave1, dmtOacute1,
    dmtOcirc1, dmtOtilde1, dmtOumlaut1, dmtDivide, dmtOslash1, dmtUgrave1,
    dmtUacute1, dmtUcirc1, dmtUumlaut1, dmtYacute1, dmtThorn1, dmtYumlaut,
    dmtFnof, dmtAlpha, dmtBeta, dmtGamma, dmtDelta, dmtEpsilon, dmtZeta, dmtEta,
    dmtTheta, dmtIota, dmtKappa, dmtLambda, dmtMu, dmtNu, dmtXi, dmtOmicron,
    dmtPi, dmtRho, dmtSigma, dmtTau, dmtUpsilon, dmtPhi, dmtChi, dmtPsi,
    dmtOmega, dmtAlpha1, dmtBeta1, dmtGamma1, dmtDelta1, dmtEpsilon1, dmtZeta1,
    dmtEta1, dmtTheta1, dmtIota1, dmtKappa1, dmtLambda1, dmtMu1, dmtNu1, dmtXi1,
    dmtOmicron1, dmtPi1, dmtRho1, dmtSigmaf, dmtSigma1, dmtTau1, dmtUpsilon1,
    dmtPhi1, dmtChi1, dmtPsi1, dmtOmega1, dmtThetasym, dmtUpsih, dmtPiv,
    dmtBull, dmtHellip, dmtPrime, dmtPrime1, dmtOline, dmtFrasl, dmtWeierp,
    dmtImaginary, dmtReal, dmtTrademark, dmtAlefsym, dmtLarr, dmtUarr, dmtRarr,
    dmtDarr, dmtHarr, dmtCrarr, dmtLArr1, dmtUArr1, dmtRArr1, dmtDArr1,
    dmtHArr1, dmtForall, dmtPart, dmtExist, dmtEmpty, dmtNabla, dmtIsin,
    dmtNotin, dmtNi, dmtProd, dmtSum, dmtMinus, dmtLowast, dmtRadic, dmtProp,
    dmtInfin, dmtAng, dmtAnd, dmtOr, dmtCap, dmtCup, dmtInt, dmtThere4, dmtSim,
    dmtCong, dmtAsymp, dmtNe, dmtEquiv, dmtLe, dmtGe, dmtSub, dmtSup, dmtNsub,
    dmtSube, dmtSupe, dmtOplus, dmtOtimes, dmtPerp, dmtSdot, dmtLceil, dmtRceil,
    dmtLfloor, dmtRfloor, dmtLang, dmtRang, dmtLoz, dmtSpades, dmtClubs,
    dmtHearts, dmtDiams, dmtOElig, dmtOelig1, dmtScaron, dmtScaron1,
    dmtYumlaut1, dmtCirc, dmtTilde, dmtEnsp, dmtEmsp, dmtThinsp, dmtZwnj,
    dmtZwj, dmtLrm, dmtRlm, dmtNdash, dmtMdash, dmtLsquo, dmtRsquo, dmtSbquo,
    dmtLdquo, dmtRdquo, dmtBdquo, dmtDagger, dmtDagger1, dmtPermil, dmtLsaquo,
    dmtRsaquo, dmtEuro, dmtTm, dmtHruler, dmtProgramlisting, dmtIndexentry,
    dmtOrderedlist, dmtItemizedlist, dmtSimplesect, dmtTitle, dmtVariablelist,
    dmtTable, dmtHeading, dmtToclist, dmtLanguage, dmtParameterlist,
    dmtXrefsect, dmtCopydoc, dmtBlockquote, dmtParblock, dmtMixedStr
  DocMarkupTypeBody* = object
    case kind*: DocMarkupTypeKind
    of dmtUlink:
        docURLLink*: DocURLLink

    of dmtBold, dmtS, dmtStrike, dmtUnderline, dmtEmphasis, dmtComputeroutput,
       dmtSubscript, dmtSuperscript, dmtCenter, dmtSmall, dmtDel, dmtIns,
       dmtPreformatted:
        docMarkupType*: DocMarkupType

    of dmtHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of dmtManonly, dmtXmlonly, dmtRtfonly, dmtLatexonly, dmtDocbookonly,
       dmtVerbatim:
        fString*: string

    of dmtImage, dmtDot, dmtMsc, dmtPlantuml, dmtDotfile, dmtMscfile, dmtDiafile:
        docImageType*: DocImageType

    of dmtAnchor:
        docAnchorType*: DocAnchorType

    of dmtFormula:
        docFormulaType*: DocFormulaType

    of dmtRef:
        docRefTextType*: DocRefTextType

    of dmtEmoji:
        docEmojiType*: DocEmojiType

    of dmtLinebreak, dmtNonbreakablespace, dmtIexcl, dmtCent, dmtPound,
       dmtCurren, dmtYen, dmtBrvbar, dmtSect, dmtUmlaut, dmtCopy, dmtOrdf,
       dmtLaquo, dmtNot, dmtShy, dmtRegistered, dmtMacr, dmtDeg, dmtPlusmn,
       dmtSup2, dmtSup3, dmtAcute, dmtMicro, dmtPara, dmtMiddot, dmtCedil,
       dmtSup1, dmtOrdm, dmtRaquo, dmtFrac14, dmtFrac12, dmtFrac34, dmtIquest,
       dmtAgrave, dmtAacute, dmtAcirc, dmtAtilde, dmtAumlaut, dmtAring,
       dmtAElig, dmtCcedil, dmtEgrave, dmtEacute, dmtEcirc, dmtEumlaut,
       dmtIgrave, dmtIacute, dmtIcirc, dmtIumlaut, dmtETH, dmtNtilde, dmtOgrave,
       dmtOacute, dmtOcirc, dmtOtilde, dmtOumlaut, dmtTimes, dmtOslash,
       dmtUgrave, dmtUacute, dmtUcirc, dmtUumlaut, dmtYacute, dmtTHORN,
       dmtSzlig, dmtAgrave1, dmtAacute1, dmtAcirc1, dmtAtilde1, dmtAumlaut1,
       dmtAring1, dmtAelig1, dmtCcedil1, dmtEgrave1, dmtEacute1, dmtEcirc1,
       dmtEumlaut1, dmtIgrave1, dmtIacute1, dmtIcirc1, dmtIumlaut1, dmtEth1,
       dmtNtilde1, dmtOgrave1, dmtOacute1, dmtOcirc1, dmtOtilde1, dmtOumlaut1,
       dmtDivide, dmtOslash1, dmtUgrave1, dmtUacute1, dmtUcirc1, dmtUumlaut1,
       dmtYacute1, dmtThorn1, dmtYumlaut, dmtFnof, dmtAlpha, dmtBeta, dmtGamma,
       dmtDelta, dmtEpsilon, dmtZeta, dmtEta, dmtTheta, dmtIota, dmtKappa,
       dmtLambda, dmtMu, dmtNu, dmtXi, dmtOmicron, dmtPi, dmtRho, dmtSigma,
       dmtTau, dmtUpsilon, dmtPhi, dmtChi, dmtPsi, dmtOmega, dmtAlpha1,
       dmtBeta1, dmtGamma1, dmtDelta1, dmtEpsilon1, dmtZeta1, dmtEta1,
       dmtTheta1, dmtIota1, dmtKappa1, dmtLambda1, dmtMu1, dmtNu1, dmtXi1,
       dmtOmicron1, dmtPi1, dmtRho1, dmtSigmaf, dmtSigma1, dmtTau1, dmtUpsilon1,
       dmtPhi1, dmtChi1, dmtPsi1, dmtOmega1, dmtThetasym, dmtUpsih, dmtPiv,
       dmtBull, dmtHellip, dmtPrime, dmtPrime1, dmtOline, dmtFrasl, dmtWeierp,
       dmtImaginary, dmtReal, dmtTrademark, dmtAlefsym, dmtLarr, dmtUarr,
       dmtRarr, dmtDarr, dmtHarr, dmtCrarr, dmtLArr1, dmtUArr1, dmtRArr1,
       dmtDArr1, dmtHArr1, dmtForall, dmtPart, dmtExist, dmtEmpty, dmtNabla,
       dmtIsin, dmtNotin, dmtNi, dmtProd, dmtSum, dmtMinus, dmtLowast, dmtRadic,
       dmtProp, dmtInfin, dmtAng, dmtAnd, dmtOr, dmtCap, dmtCup, dmtInt,
       dmtThere4, dmtSim, dmtCong, dmtAsymp, dmtNe, dmtEquiv, dmtLe, dmtGe,
       dmtSub, dmtSup, dmtNsub, dmtSube, dmtSupe, dmtOplus, dmtOtimes, dmtPerp,
       dmtSdot, dmtLceil, dmtRceil, dmtLfloor, dmtRfloor, dmtLang, dmtRang,
       dmtLoz, dmtSpades, dmtClubs, dmtHearts, dmtDiams, dmtOElig, dmtOelig1,
       dmtScaron, dmtScaron1, dmtYumlaut1, dmtCirc, dmtTilde, dmtEnsp, dmtEmsp,
       dmtThinsp, dmtZwnj, dmtZwj, dmtLrm, dmtRlm, dmtNdash, dmtMdash, dmtLsquo,
       dmtRsquo, dmtSbquo, dmtLdquo, dmtRdquo, dmtBdquo, dmtDagger, dmtDagger1,
       dmtPermil, dmtLsaquo, dmtRsaquo, dmtEuro, dmtTm, dmtHruler:
        docEmptyType*: DocEmptyType

    of dmtProgramlisting:
        listingType*: ListingType

    of dmtIndexentry:
        docIndexEntryType*: DocIndexEntryType

    of dmtOrderedlist, dmtItemizedlist:
        docListType*: DocListType

    of dmtSimplesect:
        docSimpleSectType*: DocSimpleSectType

    of dmtTitle:
        docTitleType*: DocTitleType

    of dmtVariablelist:
        docVariableListType*: DocVariableListType

    of dmtTable:
        docTableType*: DocTableType

    of dmtHeading:
        docHeadingType*: DocHeadingType

    of dmtToclist:
        docTocListType*: DocTocListType

    of dmtLanguage:
        docLanguageType*: DocLanguageType

    of dmtParameterlist:
        docParamListType*: DocParamListType

    of dmtXrefsect:
        docXRefSectType*: DocXRefSectType

    of dmtCopydoc:
        docCopyType*: DocCopyType

    of dmtBlockquote:
        docBlockQuoteType*: DocBlockQuoteType

    of dmtParblock:
        docParBlockType*: DocParBlockType

    of dmtMixedStr:
        mixedStr*: string

  
  DocMarkupType* = object
    xsdChoice*: seq[DocMarkupTypeBody]

  DocURLLinkKind* = enum
    dulUlink, dulBold, dulS, dulStrike, dulUnderline, dulEmphasis,
    dulComputeroutput, dulSubscript, dulSuperscript, dulCenter, dulSmall,
    dulDel, dulIns, dulHtmlonly, dulManonly, dulXmlonly, dulRtfonly,
    dulLatexonly, dulDocbookonly, dulImage, dulDot, dulMsc, dulPlantuml,
    dulAnchor, dulFormula, dulRef, dulEmoji, dulLinebreak, dulNonbreakablespace,
    dulIexcl, dulCent, dulPound, dulCurren, dulYen, dulBrvbar, dulSect,
    dulUmlaut, dulCopy, dulOrdf, dulLaquo, dulNot, dulShy, dulRegistered,
    dulMacr, dulDeg, dulPlusmn, dulSup2, dulSup3, dulAcute, dulMicro, dulPara,
    dulMiddot, dulCedil, dulSup1, dulOrdm, dulRaquo, dulFrac14, dulFrac12,
    dulFrac34, dulIquest, dulAgrave, dulAacute, dulAcirc, dulAtilde, dulAumlaut,
    dulAring, dulAElig, dulCcedil, dulEgrave, dulEacute, dulEcirc, dulEumlaut,
    dulIgrave, dulIacute, dulIcirc, dulIumlaut, dulETH, dulNtilde, dulOgrave,
    dulOacute, dulOcirc, dulOtilde, dulOumlaut, dulTimes, dulOslash, dulUgrave,
    dulUacute, dulUcirc, dulUumlaut, dulYacute, dulTHORN, dulSzlig, dulAgrave1,
    dulAacute1, dulAcirc1, dulAtilde1, dulAumlaut1, dulAring1, dulAelig1,
    dulCcedil1, dulEgrave1, dulEacute1, dulEcirc1, dulEumlaut1, dulIgrave1,
    dulIacute1, dulIcirc1, dulIumlaut1, dulEth1, dulNtilde1, dulOgrave1,
    dulOacute1, dulOcirc1, dulOtilde1, dulOumlaut1, dulDivide, dulOslash1,
    dulUgrave1, dulUacute1, dulUcirc1, dulUumlaut1, dulYacute1, dulThorn1,
    dulYumlaut, dulFnof, dulAlpha, dulBeta, dulGamma, dulDelta, dulEpsilon,
    dulZeta, dulEta, dulTheta, dulIota, dulKappa, dulLambda, dulMu, dulNu,
    dulXi, dulOmicron, dulPi, dulRho, dulSigma, dulTau, dulUpsilon, dulPhi,
    dulChi, dulPsi, dulOmega, dulAlpha1, dulBeta1, dulGamma1, dulDelta1,
    dulEpsilon1, dulZeta1, dulEta1, dulTheta1, dulIota1, dulKappa1, dulLambda1,
    dulMu1, dulNu1, dulXi1, dulOmicron1, dulPi1, dulRho1, dulSigmaf, dulSigma1,
    dulTau1, dulUpsilon1, dulPhi1, dulChi1, dulPsi1, dulOmega1, dulThetasym,
    dulUpsih, dulPiv, dulBull, dulHellip, dulPrime, dulPrime1, dulOline,
    dulFrasl, dulWeierp, dulImaginary, dulReal, dulTrademark, dulAlefsym,
    dulLarr, dulUarr, dulRarr, dulDarr, dulHarr, dulCrarr, dulLArr1, dulUArr1,
    dulRArr1, dulDArr1, dulHArr1, dulForall, dulPart, dulExist, dulEmpty,
    dulNabla, dulIsin, dulNotin, dulNi, dulProd, dulSum, dulMinus, dulLowast,
    dulRadic, dulProp, dulInfin, dulAng, dulAnd, dulOr, dulCap, dulCup, dulInt,
    dulThere4, dulSim, dulCong, dulAsymp, dulNe, dulEquiv, dulLe, dulGe, dulSub,
    dulSup, dulNsub, dulSube, dulSupe, dulOplus, dulOtimes, dulPerp, dulSdot,
    dulLceil, dulRceil, dulLfloor, dulRfloor, dulLang, dulRang, dulLoz,
    dulSpades, dulClubs, dulHearts, dulDiams, dulOElig, dulOelig1, dulScaron,
    dulScaron1, dulYumlaut1, dulCirc, dulTilde, dulEnsp, dulEmsp, dulThinsp,
    dulZwnj, dulZwj, dulLrm, dulRlm, dulNdash, dulMdash, dulLsquo, dulRsquo,
    dulSbquo, dulLdquo, dulRdquo, dulBdquo, dulDagger, dulDagger1, dulPermil,
    dulLsaquo, dulRsaquo, dulEuro, dulTm, dulMixedStr
  DocURLLinkBody* = object
    case kind*: DocURLLinkKind
    of dulUlink:
        docURLLink*: DocURLLink

    of dulBold, dulS, dulStrike, dulUnderline, dulEmphasis, dulComputeroutput,
       dulSubscript, dulSuperscript, dulCenter, dulSmall, dulDel, dulIns:
        docMarkupType*: DocMarkupType

    of dulHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of dulManonly, dulXmlonly, dulRtfonly, dulLatexonly, dulDocbookonly:
        fString*: string

    of dulImage, dulDot, dulMsc, dulPlantuml:
        docImageType*: DocImageType

    of dulAnchor:
        docAnchorType*: DocAnchorType

    of dulFormula:
        docFormulaType*: DocFormulaType

    of dulRef:
        docRefTextType*: DocRefTextType

    of dulEmoji:
        docEmojiType*: DocEmojiType

    of dulLinebreak, dulNonbreakablespace, dulIexcl, dulCent, dulPound,
       dulCurren, dulYen, dulBrvbar, dulSect, dulUmlaut, dulCopy, dulOrdf,
       dulLaquo, dulNot, dulShy, dulRegistered, dulMacr, dulDeg, dulPlusmn,
       dulSup2, dulSup3, dulAcute, dulMicro, dulPara, dulMiddot, dulCedil,
       dulSup1, dulOrdm, dulRaquo, dulFrac14, dulFrac12, dulFrac34, dulIquest,
       dulAgrave, dulAacute, dulAcirc, dulAtilde, dulAumlaut, dulAring,
       dulAElig, dulCcedil, dulEgrave, dulEacute, dulEcirc, dulEumlaut,
       dulIgrave, dulIacute, dulIcirc, dulIumlaut, dulETH, dulNtilde, dulOgrave,
       dulOacute, dulOcirc, dulOtilde, dulOumlaut, dulTimes, dulOslash,
       dulUgrave, dulUacute, dulUcirc, dulUumlaut, dulYacute, dulTHORN,
       dulSzlig, dulAgrave1, dulAacute1, dulAcirc1, dulAtilde1, dulAumlaut1,
       dulAring1, dulAelig1, dulCcedil1, dulEgrave1, dulEacute1, dulEcirc1,
       dulEumlaut1, dulIgrave1, dulIacute1, dulIcirc1, dulIumlaut1, dulEth1,
       dulNtilde1, dulOgrave1, dulOacute1, dulOcirc1, dulOtilde1, dulOumlaut1,
       dulDivide, dulOslash1, dulUgrave1, dulUacute1, dulUcirc1, dulUumlaut1,
       dulYacute1, dulThorn1, dulYumlaut, dulFnof, dulAlpha, dulBeta, dulGamma,
       dulDelta, dulEpsilon, dulZeta, dulEta, dulTheta, dulIota, dulKappa,
       dulLambda, dulMu, dulNu, dulXi, dulOmicron, dulPi, dulRho, dulSigma,
       dulTau, dulUpsilon, dulPhi, dulChi, dulPsi, dulOmega, dulAlpha1,
       dulBeta1, dulGamma1, dulDelta1, dulEpsilon1, dulZeta1, dulEta1,
       dulTheta1, dulIota1, dulKappa1, dulLambda1, dulMu1, dulNu1, dulXi1,
       dulOmicron1, dulPi1, dulRho1, dulSigmaf, dulSigma1, dulTau1, dulUpsilon1,
       dulPhi1, dulChi1, dulPsi1, dulOmega1, dulThetasym, dulUpsih, dulPiv,
       dulBull, dulHellip, dulPrime, dulPrime1, dulOline, dulFrasl, dulWeierp,
       dulImaginary, dulReal, dulTrademark, dulAlefsym, dulLarr, dulUarr,
       dulRarr, dulDarr, dulHarr, dulCrarr, dulLArr1, dulUArr1, dulRArr1,
       dulDArr1, dulHArr1, dulForall, dulPart, dulExist, dulEmpty, dulNabla,
       dulIsin, dulNotin, dulNi, dulProd, dulSum, dulMinus, dulLowast, dulRadic,
       dulProp, dulInfin, dulAng, dulAnd, dulOr, dulCap, dulCup, dulInt,
       dulThere4, dulSim, dulCong, dulAsymp, dulNe, dulEquiv, dulLe, dulGe,
       dulSub, dulSup, dulNsub, dulSube, dulSupe, dulOplus, dulOtimes, dulPerp,
       dulSdot, dulLceil, dulRceil, dulLfloor, dulRfloor, dulLang, dulRang,
       dulLoz, dulSpades, dulClubs, dulHearts, dulDiams, dulOElig, dulOelig1,
       dulScaron, dulScaron1, dulYumlaut1, dulCirc, dulTilde, dulEnsp, dulEmsp,
       dulThinsp, dulZwnj, dulZwj, dulLrm, dulRlm, dulNdash, dulMdash, dulLsquo,
       dulRsquo, dulSbquo, dulLdquo, dulRdquo, dulBdquo, dulDagger, dulDagger1,
       dulPermil, dulLsaquo, dulRsaquo, dulEuro, dulTm:
        docEmptyType*: DocEmptyType

    of dulMixedStr:
        mixedStr*: string

  
  DocURLLink* = object
    url*: string
    xsdChoice*: seq[DocURLLinkBody]

  DocAnchorTypeKind* = enum
    datMixedStr
  DocAnchorTypeBody* = object
    case kind*: DocAnchorTypeKind
    of datMixedStr:
        mixedStr*: string

  
  DocAnchorType* = object
    id*: string
    xsdChoice*: seq[DocAnchorTypeBody]

  DocFormulaTypeKind* = enum
    dftMixedStr
  DocFormulaTypeBody* = object
    case kind*: DocFormulaTypeKind
    of dftMixedStr:
        mixedStr*: string

  
  DocFormulaType* = object
    id*: string
    xsdChoice*: seq[DocFormulaTypeBody]

  DocIndexEntryType* = object
    primaryie*: string
    secondaryie*: string

  DocListType* = object
    listitem*: seq[DocListItemType]

  DocListItemType* = object
    para*: seq[DocParaType]

  DocSimpleSectType* = object
    kind*: DoxSimpleSectKind
    title*: Option[DocTitleType]

  DocVarListEntryType* = object
    term*: DocTitleType

  DocVariableListType* = object
    varlistentry*: DocVarListEntryType
    listitem*: DocListItemType

  DocRefTextTypeKind* = enum
    drttUlink, drttBold, drttS, drttStrike, drttUnderline, drttEmphasis,
    drttComputeroutput, drttSubscript, drttSuperscript, drttCenter, drttSmall,
    drttDel, drttIns, drttHtmlonly, drttManonly, drttXmlonly, drttRtfonly,
    drttLatexonly, drttDocbookonly, drttImage, drttDot, drttMsc, drttPlantuml,
    drttAnchor, drttFormula, drttRef, drttEmoji, drttLinebreak,
    drttNonbreakablespace, drttIexcl, drttCent, drttPound, drttCurren, drttYen,
    drttBrvbar, drttSect, drttUmlaut, drttCopy, drttOrdf, drttLaquo, drttNot,
    drttShy, drttRegistered, drttMacr, drttDeg, drttPlusmn, drttSup2, drttSup3,
    drttAcute, drttMicro, drttPara, drttMiddot, drttCedil, drttSup1, drttOrdm,
    drttRaquo, drttFrac14, drttFrac12, drttFrac34, drttIquest, drttAgrave,
    drttAacute, drttAcirc, drttAtilde, drttAumlaut, drttAring, drttAElig,
    drttCcedil, drttEgrave, drttEacute, drttEcirc, drttEumlaut, drttIgrave,
    drttIacute, drttIcirc, drttIumlaut, drttETH, drttNtilde, drttOgrave,
    drttOacute, drttOcirc, drttOtilde, drttOumlaut, drttTimes, drttOslash,
    drttUgrave, drttUacute, drttUcirc, drttUumlaut, drttYacute, drttTHORN,
    drttSzlig, drttAgrave1, drttAacute1, drttAcirc1, drttAtilde1, drttAumlaut1,
    drttAring1, drttAelig1, drttCcedil1, drttEgrave1, drttEacute1, drttEcirc1,
    drttEumlaut1, drttIgrave1, drttIacute1, drttIcirc1, drttIumlaut1, drttEth1,
    drttNtilde1, drttOgrave1, drttOacute1, drttOcirc1, drttOtilde1,
    drttOumlaut1, drttDivide, drttOslash1, drttUgrave1, drttUacute1, drttUcirc1,
    drttUumlaut1, drttYacute1, drttThorn1, drttYumlaut, drttFnof, drttAlpha,
    drttBeta, drttGamma, drttDelta, drttEpsilon, drttZeta, drttEta, drttTheta,
    drttIota, drttKappa, drttLambda, drttMu, drttNu, drttXi, drttOmicron,
    drttPi, drttRho, drttSigma, drttTau, drttUpsilon, drttPhi, drttChi, drttPsi,
    drttOmega, drttAlpha1, drttBeta1, drttGamma1, drttDelta1, drttEpsilon1,
    drttZeta1, drttEta1, drttTheta1, drttIota1, drttKappa1, drttLambda1,
    drttMu1, drttNu1, drttXi1, drttOmicron1, drttPi1, drttRho1, drttSigmaf,
    drttSigma1, drttTau1, drttUpsilon1, drttPhi1, drttChi1, drttPsi1,
    drttOmega1, drttThetasym, drttUpsih, drttPiv, drttBull, drttHellip,
    drttPrime, drttPrime1, drttOline, drttFrasl, drttWeierp, drttImaginary,
    drttReal, drttTrademark, drttAlefsym, drttLarr, drttUarr, drttRarr,
    drttDarr, drttHarr, drttCrarr, drttLArr1, drttUArr1, drttRArr1, drttDArr1,
    drttHArr1, drttForall, drttPart, drttExist, drttEmpty, drttNabla, drttIsin,
    drttNotin, drttNi, drttProd, drttSum, drttMinus, drttLowast, drttRadic,
    drttProp, drttInfin, drttAng, drttAnd, drttOr, drttCap, drttCup, drttInt,
    drttThere4, drttSim, drttCong, drttAsymp, drttNe, drttEquiv, drttLe, drttGe,
    drttSub, drttSup, drttNsub, drttSube, drttSupe, drttOplus, drttOtimes,
    drttPerp, drttSdot, drttLceil, drttRceil, drttLfloor, drttRfloor, drttLang,
    drttRang, drttLoz, drttSpades, drttClubs, drttHearts, drttDiams, drttOElig,
    drttOelig1, drttScaron, drttScaron1, drttYumlaut1, drttCirc, drttTilde,
    drttEnsp, drttEmsp, drttThinsp, drttZwnj, drttZwj, drttLrm, drttRlm,
    drttNdash, drttMdash, drttLsquo, drttRsquo, drttSbquo, drttLdquo, drttRdquo,
    drttBdquo, drttDagger, drttDagger1, drttPermil, drttLsaquo, drttRsaquo,
    drttEuro, drttTm, drttMixedStr
  DocRefTextTypeBody* = object
    case kind*: DocRefTextTypeKind
    of drttUlink:
        docURLLink*: DocURLLink

    of drttBold, drttS, drttStrike, drttUnderline, drttEmphasis,
       drttComputeroutput, drttSubscript, drttSuperscript, drttCenter,
       drttSmall, drttDel, drttIns:
        docMarkupType*: DocMarkupType

    of drttHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of drttManonly, drttXmlonly, drttRtfonly, drttLatexonly, drttDocbookonly:
        fString*: string

    of drttImage, drttDot, drttMsc, drttPlantuml:
        docImageType*: DocImageType

    of drttAnchor:
        docAnchorType*: DocAnchorType

    of drttFormula:
        docFormulaType*: DocFormulaType

    of drttRef:
        docRefTextType*: DocRefTextType

    of drttEmoji:
        docEmojiType*: DocEmojiType

    of drttLinebreak, drttNonbreakablespace, drttIexcl, drttCent, drttPound,
       drttCurren, drttYen, drttBrvbar, drttSect, drttUmlaut, drttCopy,
       drttOrdf, drttLaquo, drttNot, drttShy, drttRegistered, drttMacr, drttDeg,
       drttPlusmn, drttSup2, drttSup3, drttAcute, drttMicro, drttPara,
       drttMiddot, drttCedil, drttSup1, drttOrdm, drttRaquo, drttFrac14,
       drttFrac12, drttFrac34, drttIquest, drttAgrave, drttAacute, drttAcirc,
       drttAtilde, drttAumlaut, drttAring, drttAElig, drttCcedil, drttEgrave,
       drttEacute, drttEcirc, drttEumlaut, drttIgrave, drttIacute, drttIcirc,
       drttIumlaut, drttETH, drttNtilde, drttOgrave, drttOacute, drttOcirc,
       drttOtilde, drttOumlaut, drttTimes, drttOslash, drttUgrave, drttUacute,
       drttUcirc, drttUumlaut, drttYacute, drttTHORN, drttSzlig, drttAgrave1,
       drttAacute1, drttAcirc1, drttAtilde1, drttAumlaut1, drttAring1,
       drttAelig1, drttCcedil1, drttEgrave1, drttEacute1, drttEcirc1,
       drttEumlaut1, drttIgrave1, drttIacute1, drttIcirc1, drttIumlaut1,
       drttEth1, drttNtilde1, drttOgrave1, drttOacute1, drttOcirc1, drttOtilde1,
       drttOumlaut1, drttDivide, drttOslash1, drttUgrave1, drttUacute1,
       drttUcirc1, drttUumlaut1, drttYacute1, drttThorn1, drttYumlaut, drttFnof,
       drttAlpha, drttBeta, drttGamma, drttDelta, drttEpsilon, drttZeta,
       drttEta, drttTheta, drttIota, drttKappa, drttLambda, drttMu, drttNu,
       drttXi, drttOmicron, drttPi, drttRho, drttSigma, drttTau, drttUpsilon,
       drttPhi, drttChi, drttPsi, drttOmega, drttAlpha1, drttBeta1, drttGamma1,
       drttDelta1, drttEpsilon1, drttZeta1, drttEta1, drttTheta1, drttIota1,
       drttKappa1, drttLambda1, drttMu1, drttNu1, drttXi1, drttOmicron1,
       drttPi1, drttRho1, drttSigmaf, drttSigma1, drttTau1, drttUpsilon1,
       drttPhi1, drttChi1, drttPsi1, drttOmega1, drttThetasym, drttUpsih,
       drttPiv, drttBull, drttHellip, drttPrime, drttPrime1, drttOline,
       drttFrasl, drttWeierp, drttImaginary, drttReal, drttTrademark,
       drttAlefsym, drttLarr, drttUarr, drttRarr, drttDarr, drttHarr, drttCrarr,
       drttLArr1, drttUArr1, drttRArr1, drttDArr1, drttHArr1, drttForall,
       drttPart, drttExist, drttEmpty, drttNabla, drttIsin, drttNotin, drttNi,
       drttProd, drttSum, drttMinus, drttLowast, drttRadic, drttProp, drttInfin,
       drttAng, drttAnd, drttOr, drttCap, drttCup, drttInt, drttThere4, drttSim,
       drttCong, drttAsymp, drttNe, drttEquiv, drttLe, drttGe, drttSub, drttSup,
       drttNsub, drttSube, drttSupe, drttOplus, drttOtimes, drttPerp, drttSdot,
       drttLceil, drttRceil, drttLfloor, drttRfloor, drttLang, drttRang,
       drttLoz, drttSpades, drttClubs, drttHearts, drttDiams, drttOElig,
       drttOelig1, drttScaron, drttScaron1, drttYumlaut1, drttCirc, drttTilde,
       drttEnsp, drttEmsp, drttThinsp, drttZwnj, drttZwj, drttLrm, drttRlm,
       drttNdash, drttMdash, drttLsquo, drttRsquo, drttSbquo, drttLdquo,
       drttRdquo, drttBdquo, drttDagger, drttDagger1, drttPermil, drttLsaquo,
       drttRsaquo, drttEuro, drttTm:
        docEmptyType*: DocEmptyType

    of drttMixedStr:
        mixedStr*: string

  
  DocRefTextType* = object
    refid*: string
    kindref*: DoxRefKind
    external*: string
    xsdChoice*: seq[DocRefTextTypeBody]

  DocTableType* = object
    rows*: int
    cols*: int
    width*: string
    caption*: Option[DocCaptionType]
    row*: seq[DocRowType]

  DocRowType* = object
    entry*: seq[DocEntryType]

  DocEntryType* = object
    thead*: DoxBool
    colspan*: int
    rowspan*: int
    align*: DoxAlign
    valign*: DoxVerticalAlign
    width*: string
    class*: string
    para*: seq[DocParaType]

  DocCaptionTypeKind* = enum
    dctUlink, dctBold, dctS, dctStrike, dctUnderline, dctEmphasis,
    dctComputeroutput, dctSubscript, dctSuperscript, dctCenter, dctSmall,
    dctDel, dctIns, dctHtmlonly, dctManonly, dctXmlonly, dctRtfonly,
    dctLatexonly, dctDocbookonly, dctImage, dctDot, dctMsc, dctPlantuml,
    dctAnchor, dctFormula, dctRef, dctEmoji, dctLinebreak, dctNonbreakablespace,
    dctIexcl, dctCent, dctPound, dctCurren, dctYen, dctBrvbar, dctSect,
    dctUmlaut, dctCopy, dctOrdf, dctLaquo, dctNot, dctShy, dctRegistered,
    dctMacr, dctDeg, dctPlusmn, dctSup2, dctSup3, dctAcute, dctMicro, dctPara,
    dctMiddot, dctCedil, dctSup1, dctOrdm, dctRaquo, dctFrac14, dctFrac12,
    dctFrac34, dctIquest, dctAgrave, dctAacute, dctAcirc, dctAtilde, dctAumlaut,
    dctAring, dctAElig, dctCcedil, dctEgrave, dctEacute, dctEcirc, dctEumlaut,
    dctIgrave, dctIacute, dctIcirc, dctIumlaut, dctETH, dctNtilde, dctOgrave,
    dctOacute, dctOcirc, dctOtilde, dctOumlaut, dctTimes, dctOslash, dctUgrave,
    dctUacute, dctUcirc, dctUumlaut, dctYacute, dctTHORN, dctSzlig, dctAgrave1,
    dctAacute1, dctAcirc1, dctAtilde1, dctAumlaut1, dctAring1, dctAelig1,
    dctCcedil1, dctEgrave1, dctEacute1, dctEcirc1, dctEumlaut1, dctIgrave1,
    dctIacute1, dctIcirc1, dctIumlaut1, dctEth1, dctNtilde1, dctOgrave1,
    dctOacute1, dctOcirc1, dctOtilde1, dctOumlaut1, dctDivide, dctOslash1,
    dctUgrave1, dctUacute1, dctUcirc1, dctUumlaut1, dctYacute1, dctThorn1,
    dctYumlaut, dctFnof, dctAlpha, dctBeta, dctGamma, dctDelta, dctEpsilon,
    dctZeta, dctEta, dctTheta, dctIota, dctKappa, dctLambda, dctMu, dctNu,
    dctXi, dctOmicron, dctPi, dctRho, dctSigma, dctTau, dctUpsilon, dctPhi,
    dctChi, dctPsi, dctOmega, dctAlpha1, dctBeta1, dctGamma1, dctDelta1,
    dctEpsilon1, dctZeta1, dctEta1, dctTheta1, dctIota1, dctKappa1, dctLambda1,
    dctMu1, dctNu1, dctXi1, dctOmicron1, dctPi1, dctRho1, dctSigmaf, dctSigma1,
    dctTau1, dctUpsilon1, dctPhi1, dctChi1, dctPsi1, dctOmega1, dctThetasym,
    dctUpsih, dctPiv, dctBull, dctHellip, dctPrime, dctPrime1, dctOline,
    dctFrasl, dctWeierp, dctImaginary, dctReal, dctTrademark, dctAlefsym,
    dctLarr, dctUarr, dctRarr, dctDarr, dctHarr, dctCrarr, dctLArr1, dctUArr1,
    dctRArr1, dctDArr1, dctHArr1, dctForall, dctPart, dctExist, dctEmpty,
    dctNabla, dctIsin, dctNotin, dctNi, dctProd, dctSum, dctMinus, dctLowast,
    dctRadic, dctProp, dctInfin, dctAng, dctAnd, dctOr, dctCap, dctCup, dctInt,
    dctThere4, dctSim, dctCong, dctAsymp, dctNe, dctEquiv, dctLe, dctGe, dctSub,
    dctSup, dctNsub, dctSube, dctSupe, dctOplus, dctOtimes, dctPerp, dctSdot,
    dctLceil, dctRceil, dctLfloor, dctRfloor, dctLang, dctRang, dctLoz,
    dctSpades, dctClubs, dctHearts, dctDiams, dctOElig, dctOelig1, dctScaron,
    dctScaron1, dctYumlaut1, dctCirc, dctTilde, dctEnsp, dctEmsp, dctThinsp,
    dctZwnj, dctZwj, dctLrm, dctRlm, dctNdash, dctMdash, dctLsquo, dctRsquo,
    dctSbquo, dctLdquo, dctRdquo, dctBdquo, dctDagger, dctDagger1, dctPermil,
    dctLsaquo, dctRsaquo, dctEuro, dctTm, dctMixedStr
  DocCaptionTypeBody* = object
    case kind*: DocCaptionTypeKind
    of dctUlink:
        docURLLink*: DocURLLink

    of dctBold, dctS, dctStrike, dctUnderline, dctEmphasis, dctComputeroutput,
       dctSubscript, dctSuperscript, dctCenter, dctSmall, dctDel, dctIns:
        docMarkupType*: DocMarkupType

    of dctHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of dctManonly, dctXmlonly, dctRtfonly, dctLatexonly, dctDocbookonly:
        fString*: string

    of dctImage, dctDot, dctMsc, dctPlantuml:
        docImageType*: DocImageType

    of dctAnchor:
        docAnchorType*: DocAnchorType

    of dctFormula:
        docFormulaType*: DocFormulaType

    of dctRef:
        docRefTextType*: DocRefTextType

    of dctEmoji:
        docEmojiType*: DocEmojiType

    of dctLinebreak, dctNonbreakablespace, dctIexcl, dctCent, dctPound,
       dctCurren, dctYen, dctBrvbar, dctSect, dctUmlaut, dctCopy, dctOrdf,
       dctLaquo, dctNot, dctShy, dctRegistered, dctMacr, dctDeg, dctPlusmn,
       dctSup2, dctSup3, dctAcute, dctMicro, dctPara, dctMiddot, dctCedil,
       dctSup1, dctOrdm, dctRaquo, dctFrac14, dctFrac12, dctFrac34, dctIquest,
       dctAgrave, dctAacute, dctAcirc, dctAtilde, dctAumlaut, dctAring,
       dctAElig, dctCcedil, dctEgrave, dctEacute, dctEcirc, dctEumlaut,
       dctIgrave, dctIacute, dctIcirc, dctIumlaut, dctETH, dctNtilde, dctOgrave,
       dctOacute, dctOcirc, dctOtilde, dctOumlaut, dctTimes, dctOslash,
       dctUgrave, dctUacute, dctUcirc, dctUumlaut, dctYacute, dctTHORN,
       dctSzlig, dctAgrave1, dctAacute1, dctAcirc1, dctAtilde1, dctAumlaut1,
       dctAring1, dctAelig1, dctCcedil1, dctEgrave1, dctEacute1, dctEcirc1,
       dctEumlaut1, dctIgrave1, dctIacute1, dctIcirc1, dctIumlaut1, dctEth1,
       dctNtilde1, dctOgrave1, dctOacute1, dctOcirc1, dctOtilde1, dctOumlaut1,
       dctDivide, dctOslash1, dctUgrave1, dctUacute1, dctUcirc1, dctUumlaut1,
       dctYacute1, dctThorn1, dctYumlaut, dctFnof, dctAlpha, dctBeta, dctGamma,
       dctDelta, dctEpsilon, dctZeta, dctEta, dctTheta, dctIota, dctKappa,
       dctLambda, dctMu, dctNu, dctXi, dctOmicron, dctPi, dctRho, dctSigma,
       dctTau, dctUpsilon, dctPhi, dctChi, dctPsi, dctOmega, dctAlpha1,
       dctBeta1, dctGamma1, dctDelta1, dctEpsilon1, dctZeta1, dctEta1,
       dctTheta1, dctIota1, dctKappa1, dctLambda1, dctMu1, dctNu1, dctXi1,
       dctOmicron1, dctPi1, dctRho1, dctSigmaf, dctSigma1, dctTau1, dctUpsilon1,
       dctPhi1, dctChi1, dctPsi1, dctOmega1, dctThetasym, dctUpsih, dctPiv,
       dctBull, dctHellip, dctPrime, dctPrime1, dctOline, dctFrasl, dctWeierp,
       dctImaginary, dctReal, dctTrademark, dctAlefsym, dctLarr, dctUarr,
       dctRarr, dctDarr, dctHarr, dctCrarr, dctLArr1, dctUArr1, dctRArr1,
       dctDArr1, dctHArr1, dctForall, dctPart, dctExist, dctEmpty, dctNabla,
       dctIsin, dctNotin, dctNi, dctProd, dctSum, dctMinus, dctLowast, dctRadic,
       dctProp, dctInfin, dctAng, dctAnd, dctOr, dctCap, dctCup, dctInt,
       dctThere4, dctSim, dctCong, dctAsymp, dctNe, dctEquiv, dctLe, dctGe,
       dctSub, dctSup, dctNsub, dctSube, dctSupe, dctOplus, dctOtimes, dctPerp,
       dctSdot, dctLceil, dctRceil, dctLfloor, dctRfloor, dctLang, dctRang,
       dctLoz, dctSpades, dctClubs, dctHearts, dctDiams, dctOElig, dctOelig1,
       dctScaron, dctScaron1, dctYumlaut1, dctCirc, dctTilde, dctEnsp, dctEmsp,
       dctThinsp, dctZwnj, dctZwj, dctLrm, dctRlm, dctNdash, dctMdash, dctLsquo,
       dctRsquo, dctSbquo, dctLdquo, dctRdquo, dctBdquo, dctDagger, dctDagger1,
       dctPermil, dctLsaquo, dctRsaquo, dctEuro, dctTm:
        docEmptyType*: DocEmptyType

    of dctMixedStr:
        mixedStr*: string

  
  DocCaptionType* = object
    xsdChoice*: seq[DocCaptionTypeBody]

  DocHeadingTypeKind* = enum
    dhtUlink, dhtBold, dhtS, dhtStrike, dhtUnderline, dhtEmphasis,
    dhtComputeroutput, dhtSubscript, dhtSuperscript, dhtCenter, dhtSmall,
    dhtDel, dhtIns, dhtHtmlonly, dhtManonly, dhtXmlonly, dhtRtfonly,
    dhtLatexonly, dhtDocbookonly, dhtImage, dhtDot, dhtMsc, dhtPlantuml,
    dhtAnchor, dhtFormula, dhtRef, dhtEmoji, dhtLinebreak, dhtNonbreakablespace,
    dhtIexcl, dhtCent, dhtPound, dhtCurren, dhtYen, dhtBrvbar, dhtSect,
    dhtUmlaut, dhtCopy, dhtOrdf, dhtLaquo, dhtNot, dhtShy, dhtRegistered,
    dhtMacr, dhtDeg, dhtPlusmn, dhtSup2, dhtSup3, dhtAcute, dhtMicro, dhtPara,
    dhtMiddot, dhtCedil, dhtSup1, dhtOrdm, dhtRaquo, dhtFrac14, dhtFrac12,
    dhtFrac34, dhtIquest, dhtAgrave, dhtAacute, dhtAcirc, dhtAtilde, dhtAumlaut,
    dhtAring, dhtAElig, dhtCcedil, dhtEgrave, dhtEacute, dhtEcirc, dhtEumlaut,
    dhtIgrave, dhtIacute, dhtIcirc, dhtIumlaut, dhtETH, dhtNtilde, dhtOgrave,
    dhtOacute, dhtOcirc, dhtOtilde, dhtOumlaut, dhtTimes, dhtOslash, dhtUgrave,
    dhtUacute, dhtUcirc, dhtUumlaut, dhtYacute, dhtTHORN, dhtSzlig, dhtAgrave1,
    dhtAacute1, dhtAcirc1, dhtAtilde1, dhtAumlaut1, dhtAring1, dhtAelig1,
    dhtCcedil1, dhtEgrave1, dhtEacute1, dhtEcirc1, dhtEumlaut1, dhtIgrave1,
    dhtIacute1, dhtIcirc1, dhtIumlaut1, dhtEth1, dhtNtilde1, dhtOgrave1,
    dhtOacute1, dhtOcirc1, dhtOtilde1, dhtOumlaut1, dhtDivide, dhtOslash1,
    dhtUgrave1, dhtUacute1, dhtUcirc1, dhtUumlaut1, dhtYacute1, dhtThorn1,
    dhtYumlaut, dhtFnof, dhtAlpha, dhtBeta, dhtGamma, dhtDelta, dhtEpsilon,
    dhtZeta, dhtEta, dhtTheta, dhtIota, dhtKappa, dhtLambda, dhtMu, dhtNu,
    dhtXi, dhtOmicron, dhtPi, dhtRho, dhtSigma, dhtTau, dhtUpsilon, dhtPhi,
    dhtChi, dhtPsi, dhtOmega, dhtAlpha1, dhtBeta1, dhtGamma1, dhtDelta1,
    dhtEpsilon1, dhtZeta1, dhtEta1, dhtTheta1, dhtIota1, dhtKappa1, dhtLambda1,
    dhtMu1, dhtNu1, dhtXi1, dhtOmicron1, dhtPi1, dhtRho1, dhtSigmaf, dhtSigma1,
    dhtTau1, dhtUpsilon1, dhtPhi1, dhtChi1, dhtPsi1, dhtOmega1, dhtThetasym,
    dhtUpsih, dhtPiv, dhtBull, dhtHellip, dhtPrime, dhtPrime1, dhtOline,
    dhtFrasl, dhtWeierp, dhtImaginary, dhtReal, dhtTrademark, dhtAlefsym,
    dhtLarr, dhtUarr, dhtRarr, dhtDarr, dhtHarr, dhtCrarr, dhtLArr1, dhtUArr1,
    dhtRArr1, dhtDArr1, dhtHArr1, dhtForall, dhtPart, dhtExist, dhtEmpty,
    dhtNabla, dhtIsin, dhtNotin, dhtNi, dhtProd, dhtSum, dhtMinus, dhtLowast,
    dhtRadic, dhtProp, dhtInfin, dhtAng, dhtAnd, dhtOr, dhtCap, dhtCup, dhtInt,
    dhtThere4, dhtSim, dhtCong, dhtAsymp, dhtNe, dhtEquiv, dhtLe, dhtGe, dhtSub,
    dhtSup, dhtNsub, dhtSube, dhtSupe, dhtOplus, dhtOtimes, dhtPerp, dhtSdot,
    dhtLceil, dhtRceil, dhtLfloor, dhtRfloor, dhtLang, dhtRang, dhtLoz,
    dhtSpades, dhtClubs, dhtHearts, dhtDiams, dhtOElig, dhtOelig1, dhtScaron,
    dhtScaron1, dhtYumlaut1, dhtCirc, dhtTilde, dhtEnsp, dhtEmsp, dhtThinsp,
    dhtZwnj, dhtZwj, dhtLrm, dhtRlm, dhtNdash, dhtMdash, dhtLsquo, dhtRsquo,
    dhtSbquo, dhtLdquo, dhtRdquo, dhtBdquo, dhtDagger, dhtDagger1, dhtPermil,
    dhtLsaquo, dhtRsaquo, dhtEuro, dhtTm, dhtMixedStr
  DocHeadingTypeBody* = object
    case kind*: DocHeadingTypeKind
    of dhtUlink:
        docURLLink*: DocURLLink

    of dhtBold, dhtS, dhtStrike, dhtUnderline, dhtEmphasis, dhtComputeroutput,
       dhtSubscript, dhtSuperscript, dhtCenter, dhtSmall, dhtDel, dhtIns:
        docMarkupType*: DocMarkupType

    of dhtHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of dhtManonly, dhtXmlonly, dhtRtfonly, dhtLatexonly, dhtDocbookonly:
        fString*: string

    of dhtImage, dhtDot, dhtMsc, dhtPlantuml:
        docImageType*: DocImageType

    of dhtAnchor:
        docAnchorType*: DocAnchorType

    of dhtFormula:
        docFormulaType*: DocFormulaType

    of dhtRef:
        docRefTextType*: DocRefTextType

    of dhtEmoji:
        docEmojiType*: DocEmojiType

    of dhtLinebreak, dhtNonbreakablespace, dhtIexcl, dhtCent, dhtPound,
       dhtCurren, dhtYen, dhtBrvbar, dhtSect, dhtUmlaut, dhtCopy, dhtOrdf,
       dhtLaquo, dhtNot, dhtShy, dhtRegistered, dhtMacr, dhtDeg, dhtPlusmn,
       dhtSup2, dhtSup3, dhtAcute, dhtMicro, dhtPara, dhtMiddot, dhtCedil,
       dhtSup1, dhtOrdm, dhtRaquo, dhtFrac14, dhtFrac12, dhtFrac34, dhtIquest,
       dhtAgrave, dhtAacute, dhtAcirc, dhtAtilde, dhtAumlaut, dhtAring,
       dhtAElig, dhtCcedil, dhtEgrave, dhtEacute, dhtEcirc, dhtEumlaut,
       dhtIgrave, dhtIacute, dhtIcirc, dhtIumlaut, dhtETH, dhtNtilde, dhtOgrave,
       dhtOacute, dhtOcirc, dhtOtilde, dhtOumlaut, dhtTimes, dhtOslash,
       dhtUgrave, dhtUacute, dhtUcirc, dhtUumlaut, dhtYacute, dhtTHORN,
       dhtSzlig, dhtAgrave1, dhtAacute1, dhtAcirc1, dhtAtilde1, dhtAumlaut1,
       dhtAring1, dhtAelig1, dhtCcedil1, dhtEgrave1, dhtEacute1, dhtEcirc1,
       dhtEumlaut1, dhtIgrave1, dhtIacute1, dhtIcirc1, dhtIumlaut1, dhtEth1,
       dhtNtilde1, dhtOgrave1, dhtOacute1, dhtOcirc1, dhtOtilde1, dhtOumlaut1,
       dhtDivide, dhtOslash1, dhtUgrave1, dhtUacute1, dhtUcirc1, dhtUumlaut1,
       dhtYacute1, dhtThorn1, dhtYumlaut, dhtFnof, dhtAlpha, dhtBeta, dhtGamma,
       dhtDelta, dhtEpsilon, dhtZeta, dhtEta, dhtTheta, dhtIota, dhtKappa,
       dhtLambda, dhtMu, dhtNu, dhtXi, dhtOmicron, dhtPi, dhtRho, dhtSigma,
       dhtTau, dhtUpsilon, dhtPhi, dhtChi, dhtPsi, dhtOmega, dhtAlpha1,
       dhtBeta1, dhtGamma1, dhtDelta1, dhtEpsilon1, dhtZeta1, dhtEta1,
       dhtTheta1, dhtIota1, dhtKappa1, dhtLambda1, dhtMu1, dhtNu1, dhtXi1,
       dhtOmicron1, dhtPi1, dhtRho1, dhtSigmaf, dhtSigma1, dhtTau1, dhtUpsilon1,
       dhtPhi1, dhtChi1, dhtPsi1, dhtOmega1, dhtThetasym, dhtUpsih, dhtPiv,
       dhtBull, dhtHellip, dhtPrime, dhtPrime1, dhtOline, dhtFrasl, dhtWeierp,
       dhtImaginary, dhtReal, dhtTrademark, dhtAlefsym, dhtLarr, dhtUarr,
       dhtRarr, dhtDarr, dhtHarr, dhtCrarr, dhtLArr1, dhtUArr1, dhtRArr1,
       dhtDArr1, dhtHArr1, dhtForall, dhtPart, dhtExist, dhtEmpty, dhtNabla,
       dhtIsin, dhtNotin, dhtNi, dhtProd, dhtSum, dhtMinus, dhtLowast, dhtRadic,
       dhtProp, dhtInfin, dhtAng, dhtAnd, dhtOr, dhtCap, dhtCup, dhtInt,
       dhtThere4, dhtSim, dhtCong, dhtAsymp, dhtNe, dhtEquiv, dhtLe, dhtGe,
       dhtSub, dhtSup, dhtNsub, dhtSube, dhtSupe, dhtOplus, dhtOtimes, dhtPerp,
       dhtSdot, dhtLceil, dhtRceil, dhtLfloor, dhtRfloor, dhtLang, dhtRang,
       dhtLoz, dhtSpades, dhtClubs, dhtHearts, dhtDiams, dhtOElig, dhtOelig1,
       dhtScaron, dhtScaron1, dhtYumlaut1, dhtCirc, dhtTilde, dhtEnsp, dhtEmsp,
       dhtThinsp, dhtZwnj, dhtZwj, dhtLrm, dhtRlm, dhtNdash, dhtMdash, dhtLsquo,
       dhtRsquo, dhtSbquo, dhtLdquo, dhtRdquo, dhtBdquo, dhtDagger, dhtDagger1,
       dhtPermil, dhtLsaquo, dhtRsaquo, dhtEuro, dhtTm:
        docEmptyType*: DocEmptyType

    of dhtMixedStr:
        mixedStr*: string

  
  DocHeadingType* = object
    level*: int
    xsdChoice*: seq[DocHeadingTypeBody]

  DocImageTypeKind* = enum
    doitUlink, doitBold, doitS, doitStrike, doitUnderline, doitEmphasis,
    doitComputeroutput, doitSubscript, doitSuperscript, doitCenter, doitSmall,
    doitDel, doitIns, doitHtmlonly, doitManonly, doitXmlonly, doitRtfonly,
    doitLatexonly, doitDocbookonly, doitImage, doitDot, doitMsc, doitPlantuml,
    doitAnchor, doitFormula, doitRef, doitEmoji, doitLinebreak,
    doitNonbreakablespace, doitIexcl, doitCent, doitPound, doitCurren, doitYen,
    doitBrvbar, doitSect, doitUmlaut, doitCopy, doitOrdf, doitLaquo, doitNot,
    doitShy, doitRegistered, doitMacr, doitDeg, doitPlusmn, doitSup2, doitSup3,
    doitAcute, doitMicro, doitPara, doitMiddot, doitCedil, doitSup1, doitOrdm,
    doitRaquo, doitFrac14, doitFrac12, doitFrac34, doitIquest, doitAgrave,
    doitAacute, doitAcirc, doitAtilde, doitAumlaut, doitAring, doitAElig,
    doitCcedil, doitEgrave, doitEacute, doitEcirc, doitEumlaut, doitIgrave,
    doitIacute, doitIcirc, doitIumlaut, doitETH, doitNtilde, doitOgrave,
    doitOacute, doitOcirc, doitOtilde, doitOumlaut, doitTimes, doitOslash,
    doitUgrave, doitUacute, doitUcirc, doitUumlaut, doitYacute, doitTHORN,
    doitSzlig, doitAgrave1, doitAacute1, doitAcirc1, doitAtilde1, doitAumlaut1,
    doitAring1, doitAelig1, doitCcedil1, doitEgrave1, doitEacute1, doitEcirc1,
    doitEumlaut1, doitIgrave1, doitIacute1, doitIcirc1, doitIumlaut1, doitEth1,
    doitNtilde1, doitOgrave1, doitOacute1, doitOcirc1, doitOtilde1,
    doitOumlaut1, doitDivide, doitOslash1, doitUgrave1, doitUacute1, doitUcirc1,
    doitUumlaut1, doitYacute1, doitThorn1, doitYumlaut, doitFnof, doitAlpha,
    doitBeta, doitGamma, doitDelta, doitEpsilon, doitZeta, doitEta, doitTheta,
    doitIota, doitKappa, doitLambda, doitMu, doitNu, doitXi, doitOmicron,
    doitPi, doitRho, doitSigma, doitTau, doitUpsilon, doitPhi, doitChi, doitPsi,
    doitOmega, doitAlpha1, doitBeta1, doitGamma1, doitDelta1, doitEpsilon1,
    doitZeta1, doitEta1, doitTheta1, doitIota1, doitKappa1, doitLambda1,
    doitMu1, doitNu1, doitXi1, doitOmicron1, doitPi1, doitRho1, doitSigmaf,
    doitSigma1, doitTau1, doitUpsilon1, doitPhi1, doitChi1, doitPsi1,
    doitOmega1, doitThetasym, doitUpsih, doitPiv, doitBull, doitHellip,
    doitPrime, doitPrime1, doitOline, doitFrasl, doitWeierp, doitImaginary,
    doitReal, doitTrademark, doitAlefsym, doitLarr, doitUarr, doitRarr,
    doitDarr, doitHarr, doitCrarr, doitLArr1, doitUArr1, doitRArr1, doitDArr1,
    doitHArr1, doitForall, doitPart, doitExist, doitEmpty, doitNabla, doitIsin,
    doitNotin, doitNi, doitProd, doitSum, doitMinus, doitLowast, doitRadic,
    doitProp, doitInfin, doitAng, doitAnd, doitOr, doitCap, doitCup, doitInt,
    doitThere4, doitSim, doitCong, doitAsymp, doitNe, doitEquiv, doitLe, doitGe,
    doitSub, doitSup, doitNsub, doitSube, doitSupe, doitOplus, doitOtimes,
    doitPerp, doitSdot, doitLceil, doitRceil, doitLfloor, doitRfloor, doitLang,
    doitRang, doitLoz, doitSpades, doitClubs, doitHearts, doitDiams, doitOElig,
    doitOelig1, doitScaron, doitScaron1, doitYumlaut1, doitCirc, doitTilde,
    doitEnsp, doitEmsp, doitThinsp, doitZwnj, doitZwj, doitLrm, doitRlm,
    doitNdash, doitMdash, doitLsquo, doitRsquo, doitSbquo, doitLdquo, doitRdquo,
    doitBdquo, doitDagger, doitDagger1, doitPermil, doitLsaquo, doitRsaquo,
    doitEuro, doitTm, doitMixedStr
  DocImageTypeBody* = object
    case kind*: DocImageTypeKind
    of doitUlink:
        docURLLink*: DocURLLink

    of doitBold, doitS, doitStrike, doitUnderline, doitEmphasis,
       doitComputeroutput, doitSubscript, doitSuperscript, doitCenter,
       doitSmall, doitDel, doitIns:
        docMarkupType*: DocMarkupType

    of doitHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of doitManonly, doitXmlonly, doitRtfonly, doitLatexonly, doitDocbookonly:
        fString*: string

    of doitImage, doitDot, doitMsc, doitPlantuml:
        docImageType*: DocImageType

    of doitAnchor:
        docAnchorType*: DocAnchorType

    of doitFormula:
        docFormulaType*: DocFormulaType

    of doitRef:
        docRefTextType*: DocRefTextType

    of doitEmoji:
        docEmojiType*: DocEmojiType

    of doitLinebreak, doitNonbreakablespace, doitIexcl, doitCent, doitPound,
       doitCurren, doitYen, doitBrvbar, doitSect, doitUmlaut, doitCopy,
       doitOrdf, doitLaquo, doitNot, doitShy, doitRegistered, doitMacr, doitDeg,
       doitPlusmn, doitSup2, doitSup3, doitAcute, doitMicro, doitPara,
       doitMiddot, doitCedil, doitSup1, doitOrdm, doitRaquo, doitFrac14,
       doitFrac12, doitFrac34, doitIquest, doitAgrave, doitAacute, doitAcirc,
       doitAtilde, doitAumlaut, doitAring, doitAElig, doitCcedil, doitEgrave,
       doitEacute, doitEcirc, doitEumlaut, doitIgrave, doitIacute, doitIcirc,
       doitIumlaut, doitETH, doitNtilde, doitOgrave, doitOacute, doitOcirc,
       doitOtilde, doitOumlaut, doitTimes, doitOslash, doitUgrave, doitUacute,
       doitUcirc, doitUumlaut, doitYacute, doitTHORN, doitSzlig, doitAgrave1,
       doitAacute1, doitAcirc1, doitAtilde1, doitAumlaut1, doitAring1,
       doitAelig1, doitCcedil1, doitEgrave1, doitEacute1, doitEcirc1,
       doitEumlaut1, doitIgrave1, doitIacute1, doitIcirc1, doitIumlaut1,
       doitEth1, doitNtilde1, doitOgrave1, doitOacute1, doitOcirc1, doitOtilde1,
       doitOumlaut1, doitDivide, doitOslash1, doitUgrave1, doitUacute1,
       doitUcirc1, doitUumlaut1, doitYacute1, doitThorn1, doitYumlaut, doitFnof,
       doitAlpha, doitBeta, doitGamma, doitDelta, doitEpsilon, doitZeta,
       doitEta, doitTheta, doitIota, doitKappa, doitLambda, doitMu, doitNu,
       doitXi, doitOmicron, doitPi, doitRho, doitSigma, doitTau, doitUpsilon,
       doitPhi, doitChi, doitPsi, doitOmega, doitAlpha1, doitBeta1, doitGamma1,
       doitDelta1, doitEpsilon1, doitZeta1, doitEta1, doitTheta1, doitIota1,
       doitKappa1, doitLambda1, doitMu1, doitNu1, doitXi1, doitOmicron1,
       doitPi1, doitRho1, doitSigmaf, doitSigma1, doitTau1, doitUpsilon1,
       doitPhi1, doitChi1, doitPsi1, doitOmega1, doitThetasym, doitUpsih,
       doitPiv, doitBull, doitHellip, doitPrime, doitPrime1, doitOline,
       doitFrasl, doitWeierp, doitImaginary, doitReal, doitTrademark,
       doitAlefsym, doitLarr, doitUarr, doitRarr, doitDarr, doitHarr, doitCrarr,
       doitLArr1, doitUArr1, doitRArr1, doitDArr1, doitHArr1, doitForall,
       doitPart, doitExist, doitEmpty, doitNabla, doitIsin, doitNotin, doitNi,
       doitProd, doitSum, doitMinus, doitLowast, doitRadic, doitProp, doitInfin,
       doitAng, doitAnd, doitOr, doitCap, doitCup, doitInt, doitThere4, doitSim,
       doitCong, doitAsymp, doitNe, doitEquiv, doitLe, doitGe, doitSub, doitSup,
       doitNsub, doitSube, doitSupe, doitOplus, doitOtimes, doitPerp, doitSdot,
       doitLceil, doitRceil, doitLfloor, doitRfloor, doitLang, doitRang,
       doitLoz, doitSpades, doitClubs, doitHearts, doitDiams, doitOElig,
       doitOelig1, doitScaron, doitScaron1, doitYumlaut1, doitCirc, doitTilde,
       doitEnsp, doitEmsp, doitThinsp, doitZwnj, doitZwj, doitLrm, doitRlm,
       doitNdash, doitMdash, doitLsquo, doitRsquo, doitSbquo, doitLdquo,
       doitRdquo, doitBdquo, doitDagger, doitDagger1, doitPermil, doitLsaquo,
       doitRsaquo, doitEuro, doitTm:
        docEmptyType*: DocEmptyType

    of doitMixedStr:
        mixedStr*: string

  
  DocImageType* = object
    fType*: Option[DoxImageKind]
    name*: Option[string]
    width*: Option[string]
    height*: Option[string]
    alt*: Option[string]
    inline*: Option[DoxBool]
    caption*: Option[string]
    xsdChoice*: seq[DocImageTypeBody]

  DocTocItemTypeKind* = enum
    dtitUlink, dtitBold, dtitS, dtitStrike, dtitUnderline, dtitEmphasis,
    dtitComputeroutput, dtitSubscript, dtitSuperscript, dtitCenter, dtitSmall,
    dtitDel, dtitIns, dtitHtmlonly, dtitManonly, dtitXmlonly, dtitRtfonly,
    dtitLatexonly, dtitDocbookonly, dtitImage, dtitDot, dtitMsc, dtitPlantuml,
    dtitAnchor, dtitFormula, dtitRef, dtitEmoji, dtitLinebreak,
    dtitNonbreakablespace, dtitIexcl, dtitCent, dtitPound, dtitCurren, dtitYen,
    dtitBrvbar, dtitSect, dtitUmlaut, dtitCopy, dtitOrdf, dtitLaquo, dtitNot,
    dtitShy, dtitRegistered, dtitMacr, dtitDeg, dtitPlusmn, dtitSup2, dtitSup3,
    dtitAcute, dtitMicro, dtitPara, dtitMiddot, dtitCedil, dtitSup1, dtitOrdm,
    dtitRaquo, dtitFrac14, dtitFrac12, dtitFrac34, dtitIquest, dtitAgrave,
    dtitAacute, dtitAcirc, dtitAtilde, dtitAumlaut, dtitAring, dtitAElig,
    dtitCcedil, dtitEgrave, dtitEacute, dtitEcirc, dtitEumlaut, dtitIgrave,
    dtitIacute, dtitIcirc, dtitIumlaut, dtitETH, dtitNtilde, dtitOgrave,
    dtitOacute, dtitOcirc, dtitOtilde, dtitOumlaut, dtitTimes, dtitOslash,
    dtitUgrave, dtitUacute, dtitUcirc, dtitUumlaut, dtitYacute, dtitTHORN,
    dtitSzlig, dtitAgrave1, dtitAacute1, dtitAcirc1, dtitAtilde1, dtitAumlaut1,
    dtitAring1, dtitAelig1, dtitCcedil1, dtitEgrave1, dtitEacute1, dtitEcirc1,
    dtitEumlaut1, dtitIgrave1, dtitIacute1, dtitIcirc1, dtitIumlaut1, dtitEth1,
    dtitNtilde1, dtitOgrave1, dtitOacute1, dtitOcirc1, dtitOtilde1,
    dtitOumlaut1, dtitDivide, dtitOslash1, dtitUgrave1, dtitUacute1, dtitUcirc1,
    dtitUumlaut1, dtitYacute1, dtitThorn1, dtitYumlaut, dtitFnof, dtitAlpha,
    dtitBeta, dtitGamma, dtitDelta, dtitEpsilon, dtitZeta, dtitEta, dtitTheta,
    dtitIota, dtitKappa, dtitLambda, dtitMu, dtitNu, dtitXi, dtitOmicron,
    dtitPi, dtitRho, dtitSigma, dtitTau, dtitUpsilon, dtitPhi, dtitChi, dtitPsi,
    dtitOmega, dtitAlpha1, dtitBeta1, dtitGamma1, dtitDelta1, dtitEpsilon1,
    dtitZeta1, dtitEta1, dtitTheta1, dtitIota1, dtitKappa1, dtitLambda1,
    dtitMu1, dtitNu1, dtitXi1, dtitOmicron1, dtitPi1, dtitRho1, dtitSigmaf,
    dtitSigma1, dtitTau1, dtitUpsilon1, dtitPhi1, dtitChi1, dtitPsi1,
    dtitOmega1, dtitThetasym, dtitUpsih, dtitPiv, dtitBull, dtitHellip,
    dtitPrime, dtitPrime1, dtitOline, dtitFrasl, dtitWeierp, dtitImaginary,
    dtitReal, dtitTrademark, dtitAlefsym, dtitLarr, dtitUarr, dtitRarr,
    dtitDarr, dtitHarr, dtitCrarr, dtitLArr1, dtitUArr1, dtitRArr1, dtitDArr1,
    dtitHArr1, dtitForall, dtitPart, dtitExist, dtitEmpty, dtitNabla, dtitIsin,
    dtitNotin, dtitNi, dtitProd, dtitSum, dtitMinus, dtitLowast, dtitRadic,
    dtitProp, dtitInfin, dtitAng, dtitAnd, dtitOr, dtitCap, dtitCup, dtitInt,
    dtitThere4, dtitSim, dtitCong, dtitAsymp, dtitNe, dtitEquiv, dtitLe, dtitGe,
    dtitSub, dtitSup, dtitNsub, dtitSube, dtitSupe, dtitOplus, dtitOtimes,
    dtitPerp, dtitSdot, dtitLceil, dtitRceil, dtitLfloor, dtitRfloor, dtitLang,
    dtitRang, dtitLoz, dtitSpades, dtitClubs, dtitHearts, dtitDiams, dtitOElig,
    dtitOelig1, dtitScaron, dtitScaron1, dtitYumlaut1, dtitCirc, dtitTilde,
    dtitEnsp, dtitEmsp, dtitThinsp, dtitZwnj, dtitZwj, dtitLrm, dtitRlm,
    dtitNdash, dtitMdash, dtitLsquo, dtitRsquo, dtitSbquo, dtitLdquo, dtitRdquo,
    dtitBdquo, dtitDagger, dtitDagger1, dtitPermil, dtitLsaquo, dtitRsaquo,
    dtitEuro, dtitTm, dtitMixedStr
  DocTocItemTypeBody* = object
    case kind*: DocTocItemTypeKind
    of dtitUlink:
        docURLLink*: DocURLLink

    of dtitBold, dtitS, dtitStrike, dtitUnderline, dtitEmphasis,
       dtitComputeroutput, dtitSubscript, dtitSuperscript, dtitCenter,
       dtitSmall, dtitDel, dtitIns:
        docMarkupType*: DocMarkupType

    of dtitHtmlonly:
        docHtmlOnlyType*: DocHtmlOnlyType

    of dtitManonly, dtitXmlonly, dtitRtfonly, dtitLatexonly, dtitDocbookonly:
        fString*: string

    of dtitImage, dtitDot, dtitMsc, dtitPlantuml:
        docImageType*: DocImageType

    of dtitAnchor:
        docAnchorType*: DocAnchorType

    of dtitFormula:
        docFormulaType*: DocFormulaType

    of dtitRef:
        docRefTextType*: DocRefTextType

    of dtitEmoji:
        docEmojiType*: DocEmojiType

    of dtitLinebreak, dtitNonbreakablespace, dtitIexcl, dtitCent, dtitPound,
       dtitCurren, dtitYen, dtitBrvbar, dtitSect, dtitUmlaut, dtitCopy,
       dtitOrdf, dtitLaquo, dtitNot, dtitShy, dtitRegistered, dtitMacr, dtitDeg,
       dtitPlusmn, dtitSup2, dtitSup3, dtitAcute, dtitMicro, dtitPara,
       dtitMiddot, dtitCedil, dtitSup1, dtitOrdm, dtitRaquo, dtitFrac14,
       dtitFrac12, dtitFrac34, dtitIquest, dtitAgrave, dtitAacute, dtitAcirc,
       dtitAtilde, dtitAumlaut, dtitAring, dtitAElig, dtitCcedil, dtitEgrave,
       dtitEacute, dtitEcirc, dtitEumlaut, dtitIgrave, dtitIacute, dtitIcirc,
       dtitIumlaut, dtitETH, dtitNtilde, dtitOgrave, dtitOacute, dtitOcirc,
       dtitOtilde, dtitOumlaut, dtitTimes, dtitOslash, dtitUgrave, dtitUacute,
       dtitUcirc, dtitUumlaut, dtitYacute, dtitTHORN, dtitSzlig, dtitAgrave1,
       dtitAacute1, dtitAcirc1, dtitAtilde1, dtitAumlaut1, dtitAring1,
       dtitAelig1, dtitCcedil1, dtitEgrave1, dtitEacute1, dtitEcirc1,
       dtitEumlaut1, dtitIgrave1, dtitIacute1, dtitIcirc1, dtitIumlaut1,
       dtitEth1, dtitNtilde1, dtitOgrave1, dtitOacute1, dtitOcirc1, dtitOtilde1,
       dtitOumlaut1, dtitDivide, dtitOslash1, dtitUgrave1, dtitUacute1,
       dtitUcirc1, dtitUumlaut1, dtitYacute1, dtitThorn1, dtitYumlaut, dtitFnof,
       dtitAlpha, dtitBeta, dtitGamma, dtitDelta, dtitEpsilon, dtitZeta,
       dtitEta, dtitTheta, dtitIota, dtitKappa, dtitLambda, dtitMu, dtitNu,
       dtitXi, dtitOmicron, dtitPi, dtitRho, dtitSigma, dtitTau, dtitUpsilon,
       dtitPhi, dtitChi, dtitPsi, dtitOmega, dtitAlpha1, dtitBeta1, dtitGamma1,
       dtitDelta1, dtitEpsilon1, dtitZeta1, dtitEta1, dtitTheta1, dtitIota1,
       dtitKappa1, dtitLambda1, dtitMu1, dtitNu1, dtitXi1, dtitOmicron1,
       dtitPi1, dtitRho1, dtitSigmaf, dtitSigma1, dtitTau1, dtitUpsilon1,
       dtitPhi1, dtitChi1, dtitPsi1, dtitOmega1, dtitThetasym, dtitUpsih,
       dtitPiv, dtitBull, dtitHellip, dtitPrime, dtitPrime1, dtitOline,
       dtitFrasl, dtitWeierp, dtitImaginary, dtitReal, dtitTrademark,
       dtitAlefsym, dtitLarr, dtitUarr, dtitRarr, dtitDarr, dtitHarr, dtitCrarr,
       dtitLArr1, dtitUArr1, dtitRArr1, dtitDArr1, dtitHArr1, dtitForall,
       dtitPart, dtitExist, dtitEmpty, dtitNabla, dtitIsin, dtitNotin, dtitNi,
       dtitProd, dtitSum, dtitMinus, dtitLowast, dtitRadic, dtitProp, dtitInfin,
       dtitAng, dtitAnd, dtitOr, dtitCap, dtitCup, dtitInt, dtitThere4, dtitSim,
       dtitCong, dtitAsymp, dtitNe, dtitEquiv, dtitLe, dtitGe, dtitSub, dtitSup,
       dtitNsub, dtitSube, dtitSupe, dtitOplus, dtitOtimes, dtitPerp, dtitSdot,
       dtitLceil, dtitRceil, dtitLfloor, dtitRfloor, dtitLang, dtitRang,
       dtitLoz, dtitSpades, dtitClubs, dtitHearts, dtitDiams, dtitOElig,
       dtitOelig1, dtitScaron, dtitScaron1, dtitYumlaut1, dtitCirc, dtitTilde,
       dtitEnsp, dtitEmsp, dtitThinsp, dtitZwnj, dtitZwj, dtitLrm, dtitRlm,
       dtitNdash, dtitMdash, dtitLsquo, dtitRsquo, dtitSbquo, dtitLdquo,
       dtitRdquo, dtitBdquo, dtitDagger, dtitDagger1, dtitPermil, dtitLsaquo,
       dtitRsaquo, dtitEuro, dtitTm:
        docEmptyType*: DocEmptyType

    of dtitMixedStr:
        mixedStr*: string

  
  DocTocItemType* = object
    id*: string
    xsdChoice*: seq[DocTocItemTypeBody]

  DocTocListType* = object
    tocitem*: seq[DocTocItemType]

  DocLanguageType* = object
    langid*: string
    para*: seq[DocParaType]

  DocParamListType* = object
    kind*: DoxParamListKind
    parameteritem*: seq[DocParamListItem]

  DocParamListItem* = object
    parameternamelist*: seq[DocParamNameList]
    parameterdescription*: DescriptionType

  DocParamNameList* = object
    parametertype*: seq[DocParamType]
    parametername*: seq[DocParamName]

  DocParamTypeKind* = enum
    doptRef, doptMixedStr
  DocParamTypeBody* = object
    case kind*: DocParamTypeKind
    of doptRef:
        refTextType*: RefTextType

    of doptMixedStr:
        mixedStr*: string

  
  DocParamType* = object
    xsdChoice*: seq[DocParamTypeBody]

  DocParamNameKind* = enum
    dpnRef, dpnMixedStr
  DocParamNameBody* = object
    case kind*: DocParamNameKind
    of dpnRef:
        refTextType*: RefTextType

    of dpnMixedStr:
        mixedStr*: string

  
  DocParamName* = object
    direction*: Option[DoxParamDir]
    xsdChoice*: seq[DocParamNameBody]

  DocXRefSectType* = object
    id*: string
    xreftitle*: seq[string]
    xrefdescription*: DescriptionType

  DocCopyType* = object
    link*: string
    para*: seq[DocParaType]
    sect1*: seq[DocSect1Type]
    internal*: Option[DocInternalType]

  DocBlockQuoteType* = object
    para*: seq[DocParaType]

  DocParBlockType* = object
    para*: seq[DocParaType]

  DocEmptyType* = object
  
  TableofcontentsType* = object
    tocsect*: seq[TableofcontentsKindType]

  TableofcontentsKindType* = object
    name*: string
    reference*: string
    tableofcontents*: seq[TableofcontentsType]

  DocEmojiType* = object
    name*: string
    unicode*: string

  DoxBool* = enum
    dbYes,                  ## XSD enumeration: `yes`
    dbNo                     ## XSD enumeration: `no`
  DoxGraphRelation* = enum
    dgrInclude,             ## XSD enumeration: `include`
    dgrUsage,               ## XSD enumeration: `usage`
    dgrTemplateInstance,    ## XSD enumeration: `template-instance`
    dgrPublicInheritance,   ## XSD enumeration: `public-inheritance`
    dgrProtectedInheritance, ## XSD enumeration: `protected-inheritance`
    dgrPrivateInheritance,  ## XSD enumeration: `private-inheritance`
    dgrTypeConstraint        ## XSD enumeration: `type-constraint`
  DoxRefKind* = enum
    drkCompound,            ## XSD enumeration: `compound`
    drkMember                ## XSD enumeration: `member`
  DoxMemberKind* = enum
    dmkDefine,              ## XSD enumeration: `define`
    dmkProperty,            ## XSD enumeration: `property`
    dmkEvent,               ## XSD enumeration: `event`
    dmkVariable,            ## XSD enumeration: `variable`
    dmkTypedef,             ## XSD enumeration: `typedef`
    dmkEnum,                ## XSD enumeration: `enum`
    dmkFunction,            ## XSD enumeration: `function`
    dmkSignal,              ## XSD enumeration: `signal`
    dmkPrototype,           ## XSD enumeration: `prototype`
    dmkFriend,              ## XSD enumeration: `friend`
    dmkDcop,                ## XSD enumeration: `dcop`
    dmkSlot,                ## XSD enumeration: `slot`
    dmkInterface,           ## XSD enumeration: `interface`
    dmkService               ## XSD enumeration: `service`
  DoxProtectionKind* = enum
    dpkPublic,              ## XSD enumeration: `public`
    dpkProtected,           ## XSD enumeration: `protected`
    dpkPrivate,             ## XSD enumeration: `private`
    dpkPackage               ## XSD enumeration: `package`
  DoxRefQualifierKind* = enum
    drqkLvalue,             ## XSD enumeration: `lvalue`
    drqkRvalue               ## XSD enumeration: `rvalue`
  DoxLanguage* = enum
    dlUnknown,              ## XSD enumeration: `Unknown`
    dlIDL,                  ## XSD enumeration: `IDL`
    dlJava,                 ## XSD enumeration: `Java`
    dlCHash,                ## XSD enumeration: `C#`
    dlD,                    ## XSD enumeration: `D`
    dlPHP,                  ## XSD enumeration: `PHP`
    dlObjectiveC,           ## XSD enumeration: `Objective-C`
    dlCPlusPlus,            ## XSD enumeration: `C++`
    dlJavaScript,           ## XSD enumeration: `JavaScript`
    dlPython,               ## XSD enumeration: `Python`
    dlFortran,              ## XSD enumeration: `Fortran`
    dlVHDL,                 ## XSD enumeration: `VHDL`
    dlXML,                  ## XSD enumeration: `XML`
    dlSQL,                  ## XSD enumeration: `SQL`
    dlMarkdown               ## XSD enumeration: `Markdown`
  DoxVirtualKind* = enum
    dvkNonVirtual,          ## XSD enumeration: `non-virtual`
    dvkVirtual,             ## XSD enumeration: `virtual`
    dvkPureVirtual           ## XSD enumeration: `pure-virtual`
  DoxCompoundKind* = enum
    dckClass,               ## XSD enumeration: `class`
    dckStruct,              ## XSD enumeration: `struct`
    dckUnion,               ## XSD enumeration: `union`
    dckInterface,           ## XSD enumeration: `interface`
    dckProtocol,            ## XSD enumeration: `protocol`
    dckCategory,            ## XSD enumeration: `category`
    dckException,           ## XSD enumeration: `exception`
    dckService,             ## XSD enumeration: `service`
    dckSingleton,           ## XSD enumeration: `singleton`
    dckModule,              ## XSD enumeration: `module`
    dckType,                ## XSD enumeration: `type`
    dckFile,                ## XSD enumeration: `file`
    dckNamespace,           ## XSD enumeration: `namespace`
    dckGroup,               ## XSD enumeration: `group`
    dckPage,                ## XSD enumeration: `page`
    dckExample,             ## XSD enumeration: `example`
    dckDir                   ## XSD enumeration: `dir`
  DoxSectionKind* = enum
    dskUserDefined,         ## XSD enumeration: `user-defined`
    dskPublicType,          ## XSD enumeration: `public-type`
    dskPublicFunc,          ## XSD enumeration: `public-func`
    dskPublicAttrib,        ## XSD enumeration: `public-attrib`
    dskPublicSlot,          ## XSD enumeration: `public-slot`
    dskSignal,              ## XSD enumeration: `signal`
    dskDcopFunc,            ## XSD enumeration: `dcop-func`
    dskProperty,            ## XSD enumeration: `property`
    dskEvent,               ## XSD enumeration: `event`
    dskPublicStaticFunc,    ## XSD enumeration: `public-static-func`
    dskPublicStaticAttrib,  ## XSD enumeration: `public-static-attrib`
    dskProtectedType,       ## XSD enumeration: `protected-type`
    dskProtectedFunc,       ## XSD enumeration: `protected-func`
    dskProtectedAttrib,     ## XSD enumeration: `protected-attrib`
    dskProtectedSlot,       ## XSD enumeration: `protected-slot`
    dskProtectedStaticFunc, ## XSD enumeration: `protected-static-func`
    dskProtectedStaticAttrib, ## XSD enumeration: `protected-static-attrib`
    dskPackageType,         ## XSD enumeration: `package-type`
    dskPackageFunc,         ## XSD enumeration: `package-func`
    dskPackageAttrib,       ## XSD enumeration: `package-attrib`
    dskPackageStaticFunc,   ## XSD enumeration: `package-static-func`
    dskPackageStaticAttrib, ## XSD enumeration: `package-static-attrib`
    dskPrivateType,         ## XSD enumeration: `private-type`
    dskPrivateFunc,         ## XSD enumeration: `private-func`
    dskPrivateAttrib,       ## XSD enumeration: `private-attrib`
    dskPrivateSlot,         ## XSD enumeration: `private-slot`
    dskPrivateStaticFunc,   ## XSD enumeration: `private-static-func`
    dskPrivateStaticAttrib, ## XSD enumeration: `private-static-attrib`
    dskFriend,              ## XSD enumeration: `friend`
    dskRelated,             ## XSD enumeration: `related`
    dskDefine,              ## XSD enumeration: `define`
    dskPrototype,           ## XSD enumeration: `prototype`
    dskTypedef,             ## XSD enumeration: `typedef`
    dskEnum,                ## XSD enumeration: `enum`
    dskFunc,                ## XSD enumeration: `func`
    dskVar                   ## XSD enumeration: `var`
  DoxHighlightClass* = enum
    dhcComment,             ## XSD enumeration: `comment`
    dhcNormal,              ## XSD enumeration: `normal`
    dhcPreprocessor,        ## XSD enumeration: `preprocessor`
    dhcKeyword,             ## XSD enumeration: `keyword`
    dhcKeywordtype,         ## XSD enumeration: `keywordtype`
    dhcKeywordflow,         ## XSD enumeration: `keywordflow`
    dhcStringliteral,       ## XSD enumeration: `stringliteral`
    dhcCharliteral,         ## XSD enumeration: `charliteral`
    dhcVhdlkeyword,         ## XSD enumeration: `vhdlkeyword`
    dhcVhdllogic,           ## XSD enumeration: `vhdllogic`
    dhcVhdlchar,            ## XSD enumeration: `vhdlchar`
    dhcVhdldigit             ## XSD enumeration: `vhdldigit`
  DoxSimpleSectKind* = enum
    dsskSee,                ## XSD enumeration: `see`
    dsskReturn,             ## XSD enumeration: `return`
    dsskAuthor,             ## XSD enumeration: `author`
    dsskAuthors,            ## XSD enumeration: `authors`
    dsskVersion,            ## XSD enumeration: `version`
    dsskSince,              ## XSD enumeration: `since`
    dsskDate,               ## XSD enumeration: `date`
    dsskNote,               ## XSD enumeration: `note`
    dsskWarning,            ## XSD enumeration: `warning`
    dsskPre,                ## XSD enumeration: `pre`
    dsskPost,               ## XSD enumeration: `post`
    dsskCopyright,          ## XSD enumeration: `copyright`
    dsskInvariant,          ## XSD enumeration: `invariant`
    dsskRemark,             ## XSD enumeration: `remark`
    dsskAttention,          ## XSD enumeration: `attention`
    dsskPar,                ## XSD enumeration: `par`
    dsskRcs                  ## XSD enumeration: `rcs`
  DoxVersionNumber* = distinct string
  DoxImageKind* = enum
    dikHtml,                ## XSD enumeration: `html`
    dikLatex,               ## XSD enumeration: `latex`
    dikDocbook,             ## XSD enumeration: `docbook`
    dikRtf                   ## XSD enumeration: `rtf`
  DoxParamListKind* = enum
    dplkParam,              ## XSD enumeration: `param`
    dplkRetval,             ## XSD enumeration: `retval`
    dplkException,          ## XSD enumeration: `exception`
    dplkTemplateparam        ## XSD enumeration: `templateparam`
  DoxCharRange* = distinct string
  DoxParamDir* = enum
    dpdIn,                  ## XSD enumeration: `in`
    dpdOut,                 ## XSD enumeration: `out`
    dpdInout                 ## XSD enumeration: `inout`
  DoxAccessor* = enum
    daRetain,               ## XSD enumeration: `retain`
    daCopy,                 ## XSD enumeration: `copy`
    daAssign,               ## XSD enumeration: `assign`
    daWeak,                 ## XSD enumeration: `weak`
    daStrong,               ## XSD enumeration: `strong`
    daUnretained             ## XSD enumeration: `unretained`
  DoxAlign* = enum
    daLeft,                 ## XSD enumeration: `left`
    daRight,                ## XSD enumeration: `right`
    daCenter                 ## XSD enumeration: `center`
  DoxVerticalAlign* = enum
    dvaBottom,              ## XSD enumeration: `bottom`
    dvaTop,                 ## XSD enumeration: `top`
    dvaMiddle                ## XSD enumeration: `middle`

proc loadXml*(parser: var HXmlParser; target: var DoxygenType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var CompounddefType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var ListofallmembersType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var MemberRefType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocHtmlOnlyType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var CompoundRefType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var ReimplementType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var IncType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var RefType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var RefTextType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var SectiondefType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var MemberdefType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DescriptionType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var EnumvalueType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var TemplateparamlistType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var ParamType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var LinkedTextType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var GraphType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var NodeType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var ChildnodeType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var LinkType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var ListingType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var CodelineType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var HighlightType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var SpType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var ReferenceType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var LocationType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocSect1Type; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocSect2Type; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocSect3Type; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocSect4Type; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocInternalType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocInternalS1Type;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocInternalS2Type;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocInternalS3Type;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocInternalS4Type;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocTitleType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocParaType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocMarkupType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocURLLink; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocAnchorType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocFormulaType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocIndexEntryType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocListType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocListItemType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocSimpleSectType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocVarListEntryType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocVariableListType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocRefTextType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocTableType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocRowType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocEntryType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocCaptionType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocHeadingType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocImageType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocTocItemType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocTocListType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocLanguageType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocParamListType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocParamListItem; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocParamNameList; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocParamType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocParamName; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocXRefSectType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocCopyType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocBlockQuoteType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocParBlockType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocEmptyType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var TableofcontentsType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var TableofcontentsKindType;
              tag: string; inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DocEmojiType; tag: string;
              inMixed: bool = false)

proc loadXml*(parser: var HXmlParser; target: var DoxBool; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxGraphRelation; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxRefKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxMemberKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxProtectionKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxRefQualifierKind;
              tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxLanguage; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxVirtualKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxCompoundKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxSectionKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxHighlightClass; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxSimpleSectKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxVersionNumber; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxImageKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxParamListKind; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxCharRange; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxParamDir; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxAccessor; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxAlign; tag: string)

proc loadXml*(parser: var HXmlParser; target: var DoxVerticalAlign; tag: string)

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
      of "compounddef":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.compounddef, "compounddef")
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


proc loadXml*(parser: var HXmlParser; target: var CompounddefType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      of "kind":
        loadXml(parser, target.kind, "kind")
      of "language":
        loadXml(parser, target.language, "language")
      of "prot":
        loadXml(parser, target.prot, "prot")
      of "final":
        loadXml(parser, target.final, "final")
      of "inline":
        loadXml(parser, target.inline, "inline")
      of "sealed":
        loadXml(parser, target.sealed, "sealed")
      of "abstract":
        loadXml(parser, target.abstract, "abstract")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "compoundname":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.compoundname, "compoundname")
      of "title":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.title, "title")
      of "basecompoundref":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.basecompoundref, "basecompoundref")
      of "derivedcompoundref":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.derivedcompoundref, "derivedcompoundref")
      of "includes":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.includes, "includes")
      of "includedby":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.includedby, "includedby")
      of "incdepgraph":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.incdepgraph, "incdepgraph")
      of "invincdepgraph":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.invincdepgraph, "invincdepgraph")
      of "innerdir":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.innerdir, "innerdir")
      of "innerfile":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.innerfile, "innerfile")
      of "innerclass":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.innerclass, "innerclass")
      of "innernamespace":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.innernamespace, "innernamespace")
      of "innerpage":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.innerpage, "innerpage")
      of "innergroup":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.innergroup, "innergroup")
      of "templateparamlist":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.templateparamlist, "templateparamlist")
      of "sectiondef":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.sectiondef, "sectiondef")
      of "tableofcontents":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.tableofcontents, "tableofcontents")
      of "briefdescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.briefdescription, "briefdescription")
      of "detaileddescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.detaileddescription, "detaileddescription")
      of "inheritancegraph":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.inheritancegraph, "inheritancegraph")
      of "collaborationgraph":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.collaborationgraph, "collaborationgraph")
      of "programlisting":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.programlisting, "programlisting")
      of "location":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.location, "location")
      of "listofallmembers":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.listofallmembers, "listofallmembers")
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


proc loadXml*(parser: var HXmlParser; target: var ListofallmembersType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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


proc loadXml*(parser: var HXmlParser; target: var MemberRefType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "prot":
        loadXml(parser, target.prot, "prot")
      of "virt":
        loadXml(parser, target.virt, "virt")
      of "ambiguityscope":
        loadXml(parser, target.ambiguityscope, "ambiguityscope")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "scope":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.scope, "scope")
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


proc loadXml*(parser: var HXmlParser; target: var DocHtmlOnlyType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "block":
        loadXml(parser, target.fBlock, "block")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 576:8:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      target.baseExt = tmp
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var CompoundRefType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "prot":
        loadXml(parser, target.prot, "prot")
      of "virt":
        loadXml(parser, target.virt, "virt")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 576:8:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      target.baseExt = tmp
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var ReimplementType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 576:8:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      target.baseExt = tmp
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var IncType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "local":
        loadXml(parser, target.local, "local")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 576:8:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      target.baseExt = tmp
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var RefType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "prot":
        loadXml(parser, target.prot, "prot")
      of "inline":
        loadXml(parser, target.inline, "inline")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 576:8:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      target.baseExt = tmp
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var RefTextType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "kindref":
        loadXml(parser, target.kindref, "kindref")
      of "external":
        loadXml(parser, target.external, "external")
      of "tooltip":
        loadXml(parser, target.tooltip, "tooltip")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 576:8:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      target.baseExt = tmp
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var SectiondefType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
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
      of "header":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.header, "header")
      of "description":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.description, "description")
      of "memberdef":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.memberdef, "memberdef")
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


proc loadXml*(parser: var HXmlParser; target: var MemberdefType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "kind":
        loadXml(parser, target.kind, "kind")
      of "id":
        loadXml(parser, target.id, "id")
      of "prot":
        loadXml(parser, target.prot, "prot")
      of "static":
        loadXml(parser, target.fStatic, "static")
      of "strong":
        loadXml(parser, target.strong, "strong")
      of "const":
        loadXml(parser, target.fConst, "const")
      of "explicit":
        loadXml(parser, target.explicit, "explicit")
      of "inline":
        loadXml(parser, target.inline, "inline")
      of "refqual":
        loadXml(parser, target.refqual, "refqual")
      of "virt":
        loadXml(parser, target.virt, "virt")
      of "volatile":
        loadXml(parser, target.volatile, "volatile")
      of "mutable":
        loadXml(parser, target.mutable, "mutable")
      of "noexcept":
        loadXml(parser, target.noexcept, "noexcept")
      of "constexpr":
        loadXml(parser, target.constexpr, "constexpr")
      of "readable":
        loadXml(parser, target.readable, "readable")
      of "writable":
        loadXml(parser, target.writable, "writable")
      of "initonly":
        loadXml(parser, target.initonly, "initonly")
      of "settable":
        loadXml(parser, target.settable, "settable")
      of "privatesettable":
        loadXml(parser, target.privatesettable, "privatesettable")
      of "protectedsettable":
        loadXml(parser, target.protectedsettable, "protectedsettable")
      of "gettable":
        loadXml(parser, target.gettable, "gettable")
      of "privategettable":
        loadXml(parser, target.privategettable, "privategettable")
      of "protectedgettable":
        loadXml(parser, target.protectedgettable, "protectedgettable")
      of "final":
        loadXml(parser, target.final, "final")
      of "sealed":
        loadXml(parser, target.sealed, "sealed")
      of "new":
        loadXml(parser, target.new, "new")
      of "add":
        loadXml(parser, target.add, "add")
      of "remove":
        loadXml(parser, target.remove, "remove")
      of "raise":
        loadXml(parser, target.fRaise, "raise")
      of "optional":
        loadXml(parser, target.optional, "optional")
      of "required":
        loadXml(parser, target.required, "required")
      of "accessor":
        loadXml(parser, target.accessor, "accessor")
      of "attribute":
        loadXml(parser, target.attribute, "attribute")
      of "property":
        loadXml(parser, target.property, "property")
      of "readonly":
        loadXml(parser, target.readonly, "readonly")
      of "bound":
        loadXml(parser, target.bound, "bound")
      of "removable":
        loadXml(parser, target.removable, "removable")
      of "constrained":
        loadXml(parser, target.constrained, "constrained")
      of "transient":
        loadXml(parser, target.transient, "transient")
      of "maybevoid":
        loadXml(parser, target.maybevoid, "maybevoid")
      of "maybedefault":
        loadXml(parser, target.maybedefault, "maybedefault")
      of "maybeambiguous":
        loadXml(parser, target.maybeambiguous, "maybeambiguous")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "templateparamlist":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.templateparamlist, "templateparamlist")
      of "type":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.fType, "type")
      of "definition":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.definition, "definition")
      of "argsstring":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.argsstring, "argsstring")
      of "name":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.name, "name")
      of "read":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.read, "read")
      of "write":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.write, "write")
      of "bitfield":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.bitfield, "bitfield")
      of "reimplements":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.reimplements, "reimplements")
      of "reimplementedby":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.reimplementedby, "reimplementedby")
      of "param":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.param, "param")
      of "enumvalue":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.enumvalue, "enumvalue")
      of "initializer":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.initializer, "initializer")
      of "exceptions":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.exceptions, "exceptions")
      of "briefdescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.briefdescription, "briefdescription")
      of "detaileddescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.detaileddescription, "detaileddescription")
      of "inbodydescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.inbodydescription, "inbodydescription")
      of "location":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.location, "location")
      of "references":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.references, "references")
      of "referencedby":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.referencedby, "referencedby")
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


proc len*(item: DescriptionType): int =
  item.xsdChoice.len


iterator items*(item: DescriptionType): DescriptionTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DescriptionType; idx: int): DescriptionTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DescriptionType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DescriptionTypeBody(kind: dtMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "title":
        ## 617:12:xml_to_types.nim
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DescriptionTypeBody(kind: dtTitle, fString: tmp))
      of "para":
        ## 617:12:xml_to_types.nim
        var tmp: DocParaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DescriptionTypeBody(kind: dtPara, docParaType: tmp))
      of "internal":
        ## 617:12:xml_to_types.nim
        var tmp: DocInternalType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DescriptionTypeBody(kind: dtInternal, docInternalType: tmp))
      of "sect1":
        ## 617:12:xml_to_types.nim
        var tmp: DocSect1Type
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DescriptionTypeBody(kind: dtSect1, docSect1Type: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: EnumvalueType): int =
  item.xsdChoice.len


iterator items*(item: EnumvalueType): EnumvalueTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: EnumvalueType; idx: int): EnumvalueTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var EnumvalueType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      of "prot":
        loadXml(parser, target.prot, "prot")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, EnumvalueTypeBody(kind: etMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "name":
        ## 617:12:xml_to_types.nim
        var tmp: XmlNode
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, EnumvalueTypeBody(kind: etName, xmlNode: tmp))
      of "initializer":
        ## 617:12:xml_to_types.nim
        var tmp: LinkedTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            EnumvalueTypeBody(kind: etInitializer, linkedTextType: tmp))
      of "briefdescription", "detaileddescription":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "briefdescription":
          etBriefdescription
        of "detaileddescription":
          etDetaileddescription
        else:
          etBriefdescription
        var tmp: DescriptionType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = EnumvalueTypeBody(kind: kind)
        tmp2.descriptionType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var TemplateparamlistType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "param":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.param, "param")
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


proc loadXml*(parser: var HXmlParser; target: var ParamType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "attributes":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.attributes, "attributes")
      of "type":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.fType, "type")
      of "declname":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.declname, "declname")
      of "defname":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.defname, "defname")
      of "array":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.array, "array")
      of "defval":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.defval, "defval")
      of "typeconstraint":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.typeconstraint, "typeconstraint")
      of "briefdescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.briefdescription, "briefdescription")
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


proc len*(item: LinkedTextType): int =
  item.xsdChoice.len


iterator items*(item: LinkedTextType): LinkedTextTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: LinkedTextType; idx: int): LinkedTextTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var LinkedTextType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, LinkedTextTypeBody(kind: lttMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: RefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, LinkedTextTypeBody(kind: lttRef, refTextType: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var GraphType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "node":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.node, "node")
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


proc loadXml*(parser: var HXmlParser; target: var NodeType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "label":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.label, "label")
      of "link":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.link, "link")
      of "childnode":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.childnode, "childnode")
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


proc loadXml*(parser: var HXmlParser; target: var ChildnodeType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "relation":
        loadXml(parser, target.relation, "relation")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "edgelabel":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.edgelabel, "edgelabel")
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


proc loadXml*(parser: var HXmlParser; target: var LinkType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "external":
        loadXml(parser, target.external, "external")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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


proc loadXml*(parser: var HXmlParser; target: var ListingType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "filename":
        loadXml(parser, target.filename, "filename")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "codeline":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.codeline, "codeline")
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


proc loadXml*(parser: var HXmlParser; target: var CodelineType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "lineno":
        loadXml(parser, target.lineno, "lineno")
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "refkind":
        loadXml(parser, target.refkind, "refkind")
      of "external":
        loadXml(parser, target.external, "external")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "highlight":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.highlight, "highlight")
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


proc len*(item: HighlightType): int =
  item.xsdChoice.len


iterator items*(item: HighlightType): HighlightTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: HighlightType; idx: int): HighlightTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var HighlightType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "class":
        loadXml(parser, target.class, "class")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, HighlightTypeBody(kind: htMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "sp":
        ## 617:12:xml_to_types.nim
        var tmp: SpType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, HighlightTypeBody(kind: htSp, spType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: RefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, HighlightTypeBody(kind: htRef, refTextType: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: SpType): int =
  item.xsdChoice.len


iterator items*(item: SpType): SpTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: SpType; idx: int): SpTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var SpType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "value":
        loadXml(parser, target.value, "value")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, SpTypeBody(kind: stMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: ReferenceType): int =
  item.xsdChoice.len


iterator items*(item: ReferenceType): ReferenceTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: ReferenceType; idx: int): ReferenceTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var ReferenceType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "compoundref":
        loadXml(parser, target.compoundref, "compoundref")
      of "startline":
        loadXml(parser, target.startline, "startline")
      of "endline":
        loadXml(parser, target.endline, "endline")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, ReferenceTypeBody(kind: rtMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var LocationType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "file":
        loadXml(parser, target.file, "file")
      of "line":
        loadXml(parser, target.line, "line")
      of "column":
        loadXml(parser, target.column, "column")
      of "declfile":
        loadXml(parser, target.declfile, "declfile")
      of "declline":
        loadXml(parser, target.declline, "declline")
      of "declcolumn":
        loadXml(parser, target.declcolumn, "declcolumn")
      of "bodyfile":
        loadXml(parser, target.bodyfile, "bodyfile")
      of "bodystart":
        loadXml(parser, target.bodystart, "bodystart")
      of "bodyend":
        loadXml(parser, target.bodyend, "bodyend")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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


proc len*(item: DocSect1Type): int =
  item.xsdChoice.len


iterator items*(item: DocSect1Type): DocSect1TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocSect1Type; idx: int): DocSect1TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocSect1Type; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocSect1TypeBody(kind: dstMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "title":
        ## 617:12:xml_to_types.nim
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocSect1TypeBody(kind: dstTitle, fString: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocSect2Type): int =
  item.xsdChoice.len


iterator items*(item: DocSect2Type): DocSect2TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocSect2Type; idx: int): DocSect2TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocSect2Type; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocSect2TypeBody(kind: dostMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "title":
        ## 617:12:xml_to_types.nim
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocSect2TypeBody(kind: dostTitle, fString: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocSect3Type): int =
  item.xsdChoice.len


iterator items*(item: DocSect3Type): DocSect3TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocSect3Type; idx: int): DocSect3TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocSect3Type; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocSect3TypeBody(kind: docstMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "title":
        ## 617:12:xml_to_types.nim
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocSect3TypeBody(kind: docstTitle, fString: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocSect4Type): int =
  item.xsdChoice.len


iterator items*(item: DocSect4Type): DocSect4TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocSect4Type; idx: int): DocSect4TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocSect4Type; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocSect4TypeBody(kind: docsetMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "title":
        ## 617:12:xml_to_types.nim
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocSect4TypeBody(kind: docsetTitle, fString: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocInternalType): int =
  item.xsdChoice.len


iterator items*(item: DocInternalType): DocInternalTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocInternalType; idx: int): DocInternalTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocInternalType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocInternalTypeBody(kind: ditMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 617:12:xml_to_types.nim
        var tmp: DocParaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalTypeBody(kind: ditPara, docParaType: tmp))
      of "sect1":
        ## 617:12:xml_to_types.nim
        var tmp: DocSect1Type
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalTypeBody(kind: ditSect1, docSect1Type: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocInternalS1Type): int =
  item.xsdChoice.len


iterator items*(item: DocInternalS1Type): DocInternalS1TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocInternalS1Type; idx: int): DocInternalS1TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocInternalS1Type;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocInternalS1TypeBody(kind: distMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 617:12:xml_to_types.nim
        var tmp: DocParaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalS1TypeBody(kind: distPara, docParaType: tmp))
      of "sect2":
        ## 617:12:xml_to_types.nim
        var tmp: DocSect2Type
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalS1TypeBody(kind: distSect2, docSect2Type: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocInternalS2Type): int =
  item.xsdChoice.len


iterator items*(item: DocInternalS2Type): DocInternalS2TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocInternalS2Type; idx: int): DocInternalS2TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocInternalS2Type;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocInternalS2TypeBody(kind: dis2tMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 617:12:xml_to_types.nim
        var tmp: DocParaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalS2TypeBody(kind: dis2tPara, docParaType: tmp))
      of "sect3":
        ## 617:12:xml_to_types.nim
        var tmp: DocSect3Type
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalS2TypeBody(kind: dis2tSect3, docSect3Type: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocInternalS3Type): int =
  item.xsdChoice.len


iterator items*(item: DocInternalS3Type): DocInternalS3TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocInternalS3Type; idx: int): DocInternalS3TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocInternalS3Type;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocInternalS3TypeBody(kind: dis3tMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 617:12:xml_to_types.nim
        var tmp: DocParaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalS3TypeBody(kind: dis3tPara, docParaType: tmp))
      of "sect3":
        ## 617:12:xml_to_types.nim
        var tmp: DocSect4Type
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalS3TypeBody(kind: dis3tSect3, docSect4Type: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocInternalS4Type): int =
  item.xsdChoice.len


iterator items*(item: DocInternalS4Type): DocInternalS4TypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocInternalS4Type; idx: int): DocInternalS4TypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocInternalS4Type;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocInternalS4TypeBody(kind: dis4tMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 617:12:xml_to_types.nim
        var tmp: DocParaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocInternalS4TypeBody(kind: dis4tPara, docParaType: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocTitleType): int =
  item.xsdChoice.len


iterator items*(item: DocTitleType): DocTitleTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocTitleType; idx: int): DocTitleTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocTitleType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocTitleTypeBody(kind: dttMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocTitleTypeBody(kind: dttUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          dttBold
        of "s":
          dttS
        of "strike":
          dttStrike
        of "underline":
          dttUnderline
        of "emphasis":
          dttEmphasis
        of "computeroutput":
          dttComputeroutput
        of "subscript":
          dttSubscript
        of "superscript":
          dttSuperscript
        of "center":
          dttCenter
        of "small":
          dttSmall
        of "del":
          dttDel
        of "ins":
          dttIns
        else:
          dttBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTitleTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTitleTypeBody(kind: dttHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          dttManonly
        of "xmlonly":
          dttXmlonly
        of "rtfonly":
          dttRtfonly
        of "latexonly":
          dttLatexonly
        of "docbookonly":
          dttDocbookonly
        else:
          dttManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTitleTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          dttImage
        of "dot":
          dttDot
        of "msc":
          dttMsc
        of "plantuml":
          dttPlantuml
        else:
          dttImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTitleTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTitleTypeBody(kind: dttAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTitleTypeBody(kind: dttFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTitleTypeBody(kind: dttRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTitleTypeBody(kind: dttEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          dttLinebreak
        of "nonbreakablespace":
          dttNonbreakablespace
        of "iexcl":
          dttIexcl
        of "cent":
          dttCent
        of "pound":
          dttPound
        of "curren":
          dttCurren
        of "yen":
          dttYen
        of "brvbar":
          dttBrvbar
        of "sect":
          dttSect
        of "umlaut":
          dttUmlaut
        of "copy":
          dttCopy
        of "ordf":
          dttOrdf
        of "laquo":
          dttLaquo
        of "not":
          dttNot
        of "shy":
          dttShy
        of "registered":
          dttRegistered
        of "macr":
          dttMacr
        of "deg":
          dttDeg
        of "plusmn":
          dttPlusmn
        of "sup2":
          dttSup2
        of "sup3":
          dttSup3
        of "acute":
          dttAcute
        of "micro":
          dttMicro
        of "para":
          dttPara
        of "middot":
          dttMiddot
        of "cedil":
          dttCedil
        of "sup1":
          dttSup1
        of "ordm":
          dttOrdm
        of "raquo":
          dttRaquo
        of "frac14":
          dttFrac14
        of "frac12":
          dttFrac12
        of "frac34":
          dttFrac34
        of "iquest":
          dttIquest
        of "Agrave":
          dttAgrave
        of "Aacute":
          dttAacute
        of "Acirc":
          dttAcirc
        of "Atilde":
          dttAtilde
        of "Aumlaut":
          dttAumlaut
        of "Aring":
          dttAring
        of "AElig":
          dttAElig
        of "Ccedil":
          dttCcedil
        of "Egrave":
          dttEgrave
        of "Eacute":
          dttEacute
        of "Ecirc":
          dttEcirc
        of "Eumlaut":
          dttEumlaut
        of "Igrave":
          dttIgrave
        of "Iacute":
          dttIacute
        of "Icirc":
          dttIcirc
        of "Iumlaut":
          dttIumlaut
        of "ETH":
          dttETH
        of "Ntilde":
          dttNtilde
        of "Ograve":
          dttOgrave
        of "Oacute":
          dttOacute
        of "Ocirc":
          dttOcirc
        of "Otilde":
          dttOtilde
        of "Oumlaut":
          dttOumlaut
        of "times":
          dttTimes
        of "Oslash":
          dttOslash
        of "Ugrave":
          dttUgrave
        of "Uacute":
          dttUacute
        of "Ucirc":
          dttUcirc
        of "Uumlaut":
          dttUumlaut
        of "Yacute":
          dttYacute
        of "THORN":
          dttTHORN
        of "szlig":
          dttSzlig
        of "agrave":
          dttAgrave1
        of "aacute":
          dttAacute1
        of "acirc":
          dttAcirc1
        of "atilde":
          dttAtilde1
        of "aumlaut":
          dttAumlaut1
        of "aring":
          dttAring1
        of "aelig":
          dttAelig1
        of "ccedil":
          dttCcedil1
        of "egrave":
          dttEgrave1
        of "eacute":
          dttEacute1
        of "ecirc":
          dttEcirc1
        of "eumlaut":
          dttEumlaut1
        of "igrave":
          dttIgrave1
        of "iacute":
          dttIacute1
        of "icirc":
          dttIcirc1
        of "iumlaut":
          dttIumlaut1
        of "eth":
          dttEth1
        of "ntilde":
          dttNtilde1
        of "ograve":
          dttOgrave1
        of "oacute":
          dttOacute1
        of "ocirc":
          dttOcirc1
        of "otilde":
          dttOtilde1
        of "oumlaut":
          dttOumlaut1
        of "divide":
          dttDivide
        of "oslash":
          dttOslash1
        of "ugrave":
          dttUgrave1
        of "uacute":
          dttUacute1
        of "ucirc":
          dttUcirc1
        of "uumlaut":
          dttUumlaut1
        of "yacute":
          dttYacute1
        of "thorn":
          dttThorn1
        of "yumlaut":
          dttYumlaut
        of "fnof":
          dttFnof
        of "Alpha":
          dttAlpha
        of "Beta":
          dttBeta
        of "Gamma":
          dttGamma
        of "Delta":
          dttDelta
        of "Epsilon":
          dttEpsilon
        of "Zeta":
          dttZeta
        of "Eta":
          dttEta
        of "Theta":
          dttTheta
        of "Iota":
          dttIota
        of "Kappa":
          dttKappa
        of "Lambda":
          dttLambda
        of "Mu":
          dttMu
        of "Nu":
          dttNu
        of "Xi":
          dttXi
        of "Omicron":
          dttOmicron
        of "Pi":
          dttPi
        of "Rho":
          dttRho
        of "Sigma":
          dttSigma
        of "Tau":
          dttTau
        of "Upsilon":
          dttUpsilon
        of "Phi":
          dttPhi
        of "Chi":
          dttChi
        of "Psi":
          dttPsi
        of "Omega":
          dttOmega
        of "alpha":
          dttAlpha1
        of "beta":
          dttBeta1
        of "gamma":
          dttGamma1
        of "delta":
          dttDelta1
        of "epsilon":
          dttEpsilon1
        of "zeta":
          dttZeta1
        of "eta":
          dttEta1
        of "theta":
          dttTheta1
        of "iota":
          dttIota1
        of "kappa":
          dttKappa1
        of "lambda":
          dttLambda1
        of "mu":
          dttMu1
        of "nu":
          dttNu1
        of "xi":
          dttXi1
        of "omicron":
          dttOmicron1
        of "pi":
          dttPi1
        of "rho":
          dttRho1
        of "sigmaf":
          dttSigmaf
        of "sigma":
          dttSigma1
        of "tau":
          dttTau1
        of "upsilon":
          dttUpsilon1
        of "phi":
          dttPhi1
        of "chi":
          dttChi1
        of "psi":
          dttPsi1
        of "omega":
          dttOmega1
        of "thetasym":
          dttThetasym
        of "upsih":
          dttUpsih
        of "piv":
          dttPiv
        of "bull":
          dttBull
        of "hellip":
          dttHellip
        of "prime":
          dttPrime
        of "Prime":
          dttPrime1
        of "oline":
          dttOline
        of "frasl":
          dttFrasl
        of "weierp":
          dttWeierp
        of "imaginary":
          dttImaginary
        of "real":
          dttReal
        of "trademark":
          dttTrademark
        of "alefsym":
          dttAlefsym
        of "larr":
          dttLarr
        of "uarr":
          dttUarr
        of "rarr":
          dttRarr
        of "darr":
          dttDarr
        of "harr":
          dttHarr
        of "crarr":
          dttCrarr
        of "lArr":
          dttLArr1
        of "uArr":
          dttUArr1
        of "rArr":
          dttRArr1
        of "dArr":
          dttDArr1
        of "hArr":
          dttHArr1
        of "forall":
          dttForall
        of "part":
          dttPart
        of "exist":
          dttExist
        of "empty":
          dttEmpty
        of "nabla":
          dttNabla
        of "isin":
          dttIsin
        of "notin":
          dttNotin
        of "ni":
          dttNi
        of "prod":
          dttProd
        of "sum":
          dttSum
        of "minus":
          dttMinus
        of "lowast":
          dttLowast
        of "radic":
          dttRadic
        of "prop":
          dttProp
        of "infin":
          dttInfin
        of "ang":
          dttAng
        of "and":
          dttAnd
        of "or":
          dttOr
        of "cap":
          dttCap
        of "cup":
          dttCup
        of "int":
          dttInt
        of "there4":
          dttThere4
        of "sim":
          dttSim
        of "cong":
          dttCong
        of "asymp":
          dttAsymp
        of "ne":
          dttNe
        of "equiv":
          dttEquiv
        of "le":
          dttLe
        of "ge":
          dttGe
        of "sub":
          dttSub
        of "sup":
          dttSup
        of "nsub":
          dttNsub
        of "sube":
          dttSube
        of "supe":
          dttSupe
        of "oplus":
          dttOplus
        of "otimes":
          dttOtimes
        of "perp":
          dttPerp
        of "sdot":
          dttSdot
        of "lceil":
          dttLceil
        of "rceil":
          dttRceil
        of "lfloor":
          dttLfloor
        of "rfloor":
          dttRfloor
        of "lang":
          dttLang
        of "rang":
          dttRang
        of "loz":
          dttLoz
        of "spades":
          dttSpades
        of "clubs":
          dttClubs
        of "hearts":
          dttHearts
        of "diams":
          dttDiams
        of "OElig":
          dttOElig
        of "oelig":
          dttOelig1
        of "Scaron":
          dttScaron
        of "scaron":
          dttScaron1
        of "Yumlaut":
          dttYumlaut1
        of "circ":
          dttCirc
        of "tilde":
          dttTilde
        of "ensp":
          dttEnsp
        of "emsp":
          dttEmsp
        of "thinsp":
          dttThinsp
        of "zwnj":
          dttZwnj
        of "zwj":
          dttZwj
        of "lrm":
          dttLrm
        of "rlm":
          dttRlm
        of "ndash":
          dttNdash
        of "mdash":
          dttMdash
        of "lsquo":
          dttLsquo
        of "rsquo":
          dttRsquo
        of "sbquo":
          dttSbquo
        of "ldquo":
          dttLdquo
        of "rdquo":
          dttRdquo
        of "bdquo":
          dttBdquo
        of "dagger":
          dttDagger
        of "Dagger":
          dttDagger1
        of "permil":
          dttPermil
        of "lsaquo":
          dttLsaquo
        of "rsaquo":
          dttRsaquo
        of "euro":
          dttEuro
        of "tm":
          dttTm
        else:
          dttLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTitleTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocParaType): int =
  item.xsdChoice.len


iterator items*(item: DocParaType): DocParaTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocParaType; idx: int): DocParaTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocParaType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocParaTypeBody(kind: dptMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocParaTypeBody(kind: dptUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins",
         "preformatted":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          dptBold
        of "s":
          dptS
        of "strike":
          dptStrike
        of "underline":
          dptUnderline
        of "emphasis":
          dptEmphasis
        of "computeroutput":
          dptComputeroutput
        of "subscript":
          dptSubscript
        of "superscript":
          dptSuperscript
        of "center":
          dptCenter
        of "small":
          dptSmall
        of "del":
          dptDel
        of "ins":
          dptIns
        of "preformatted":
          dptPreformatted
        else:
          dptBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocParaTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly", "verbatim":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          dptManonly
        of "xmlonly":
          dptXmlonly
        of "rtfonly":
          dptRtfonly
        of "latexonly":
          dptLatexonly
        of "docbookonly":
          dptDocbookonly
        of "verbatim":
          dptVerbatim
        else:
          dptManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocParaTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml", "dotfile", "mscfile", "diafile":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          dptImage
        of "dot":
          dptDot
        of "msc":
          dptMsc
        of "plantuml":
          dptPlantuml
        of "dotfile":
          dptDotfile
        of "mscfile":
          dptMscfile
        of "diafile":
          dptDiafile
        else:
          dptImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocParaTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocParaTypeBody(kind: dptRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocParaTypeBody(kind: dptEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm",
         "hruler":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          dptLinebreak
        of "nonbreakablespace":
          dptNonbreakablespace
        of "iexcl":
          dptIexcl
        of "cent":
          dptCent
        of "pound":
          dptPound
        of "curren":
          dptCurren
        of "yen":
          dptYen
        of "brvbar":
          dptBrvbar
        of "sect":
          dptSect
        of "umlaut":
          dptUmlaut
        of "copy":
          dptCopy
        of "ordf":
          dptOrdf
        of "laquo":
          dptLaquo
        of "not":
          dptNot
        of "shy":
          dptShy
        of "registered":
          dptRegistered
        of "macr":
          dptMacr
        of "deg":
          dptDeg
        of "plusmn":
          dptPlusmn
        of "sup2":
          dptSup2
        of "sup3":
          dptSup3
        of "acute":
          dptAcute
        of "micro":
          dptMicro
        of "para":
          dptPara
        of "middot":
          dptMiddot
        of "cedil":
          dptCedil
        of "sup1":
          dptSup1
        of "ordm":
          dptOrdm
        of "raquo":
          dptRaquo
        of "frac14":
          dptFrac14
        of "frac12":
          dptFrac12
        of "frac34":
          dptFrac34
        of "iquest":
          dptIquest
        of "Agrave":
          dptAgrave
        of "Aacute":
          dptAacute
        of "Acirc":
          dptAcirc
        of "Atilde":
          dptAtilde
        of "Aumlaut":
          dptAumlaut
        of "Aring":
          dptAring
        of "AElig":
          dptAElig
        of "Ccedil":
          dptCcedil
        of "Egrave":
          dptEgrave
        of "Eacute":
          dptEacute
        of "Ecirc":
          dptEcirc
        of "Eumlaut":
          dptEumlaut
        of "Igrave":
          dptIgrave
        of "Iacute":
          dptIacute
        of "Icirc":
          dptIcirc
        of "Iumlaut":
          dptIumlaut
        of "ETH":
          dptETH
        of "Ntilde":
          dptNtilde
        of "Ograve":
          dptOgrave
        of "Oacute":
          dptOacute
        of "Ocirc":
          dptOcirc
        of "Otilde":
          dptOtilde
        of "Oumlaut":
          dptOumlaut
        of "times":
          dptTimes
        of "Oslash":
          dptOslash
        of "Ugrave":
          dptUgrave
        of "Uacute":
          dptUacute
        of "Ucirc":
          dptUcirc
        of "Uumlaut":
          dptUumlaut
        of "Yacute":
          dptYacute
        of "THORN":
          dptTHORN
        of "szlig":
          dptSzlig
        of "agrave":
          dptAgrave1
        of "aacute":
          dptAacute1
        of "acirc":
          dptAcirc1
        of "atilde":
          dptAtilde1
        of "aumlaut":
          dptAumlaut1
        of "aring":
          dptAring1
        of "aelig":
          dptAelig1
        of "ccedil":
          dptCcedil1
        of "egrave":
          dptEgrave1
        of "eacute":
          dptEacute1
        of "ecirc":
          dptEcirc1
        of "eumlaut":
          dptEumlaut1
        of "igrave":
          dptIgrave1
        of "iacute":
          dptIacute1
        of "icirc":
          dptIcirc1
        of "iumlaut":
          dptIumlaut1
        of "eth":
          dptEth1
        of "ntilde":
          dptNtilde1
        of "ograve":
          dptOgrave1
        of "oacute":
          dptOacute1
        of "ocirc":
          dptOcirc1
        of "otilde":
          dptOtilde1
        of "oumlaut":
          dptOumlaut1
        of "divide":
          dptDivide
        of "oslash":
          dptOslash1
        of "ugrave":
          dptUgrave1
        of "uacute":
          dptUacute1
        of "ucirc":
          dptUcirc1
        of "uumlaut":
          dptUumlaut1
        of "yacute":
          dptYacute1
        of "thorn":
          dptThorn1
        of "yumlaut":
          dptYumlaut
        of "fnof":
          dptFnof
        of "Alpha":
          dptAlpha
        of "Beta":
          dptBeta
        of "Gamma":
          dptGamma
        of "Delta":
          dptDelta
        of "Epsilon":
          dptEpsilon
        of "Zeta":
          dptZeta
        of "Eta":
          dptEta
        of "Theta":
          dptTheta
        of "Iota":
          dptIota
        of "Kappa":
          dptKappa
        of "Lambda":
          dptLambda
        of "Mu":
          dptMu
        of "Nu":
          dptNu
        of "Xi":
          dptXi
        of "Omicron":
          dptOmicron
        of "Pi":
          dptPi
        of "Rho":
          dptRho
        of "Sigma":
          dptSigma
        of "Tau":
          dptTau
        of "Upsilon":
          dptUpsilon
        of "Phi":
          dptPhi
        of "Chi":
          dptChi
        of "Psi":
          dptPsi
        of "Omega":
          dptOmega
        of "alpha":
          dptAlpha1
        of "beta":
          dptBeta1
        of "gamma":
          dptGamma1
        of "delta":
          dptDelta1
        of "epsilon":
          dptEpsilon1
        of "zeta":
          dptZeta1
        of "eta":
          dptEta1
        of "theta":
          dptTheta1
        of "iota":
          dptIota1
        of "kappa":
          dptKappa1
        of "lambda":
          dptLambda1
        of "mu":
          dptMu1
        of "nu":
          dptNu1
        of "xi":
          dptXi1
        of "omicron":
          dptOmicron1
        of "pi":
          dptPi1
        of "rho":
          dptRho1
        of "sigmaf":
          dptSigmaf
        of "sigma":
          dptSigma1
        of "tau":
          dptTau1
        of "upsilon":
          dptUpsilon1
        of "phi":
          dptPhi1
        of "chi":
          dptChi1
        of "psi":
          dptPsi1
        of "omega":
          dptOmega1
        of "thetasym":
          dptThetasym
        of "upsih":
          dptUpsih
        of "piv":
          dptPiv
        of "bull":
          dptBull
        of "hellip":
          dptHellip
        of "prime":
          dptPrime
        of "Prime":
          dptPrime1
        of "oline":
          dptOline
        of "frasl":
          dptFrasl
        of "weierp":
          dptWeierp
        of "imaginary":
          dptImaginary
        of "real":
          dptReal
        of "trademark":
          dptTrademark
        of "alefsym":
          dptAlefsym
        of "larr":
          dptLarr
        of "uarr":
          dptUarr
        of "rarr":
          dptRarr
        of "darr":
          dptDarr
        of "harr":
          dptHarr
        of "crarr":
          dptCrarr
        of "lArr":
          dptLArr1
        of "uArr":
          dptUArr1
        of "rArr":
          dptRArr1
        of "dArr":
          dptDArr1
        of "hArr":
          dptHArr1
        of "forall":
          dptForall
        of "part":
          dptPart
        of "exist":
          dptExist
        of "empty":
          dptEmpty
        of "nabla":
          dptNabla
        of "isin":
          dptIsin
        of "notin":
          dptNotin
        of "ni":
          dptNi
        of "prod":
          dptProd
        of "sum":
          dptSum
        of "minus":
          dptMinus
        of "lowast":
          dptLowast
        of "radic":
          dptRadic
        of "prop":
          dptProp
        of "infin":
          dptInfin
        of "ang":
          dptAng
        of "and":
          dptAnd
        of "or":
          dptOr
        of "cap":
          dptCap
        of "cup":
          dptCup
        of "int":
          dptInt
        of "there4":
          dptThere4
        of "sim":
          dptSim
        of "cong":
          dptCong
        of "asymp":
          dptAsymp
        of "ne":
          dptNe
        of "equiv":
          dptEquiv
        of "le":
          dptLe
        of "ge":
          dptGe
        of "sub":
          dptSub
        of "sup":
          dptSup
        of "nsub":
          dptNsub
        of "sube":
          dptSube
        of "supe":
          dptSupe
        of "oplus":
          dptOplus
        of "otimes":
          dptOtimes
        of "perp":
          dptPerp
        of "sdot":
          dptSdot
        of "lceil":
          dptLceil
        of "rceil":
          dptRceil
        of "lfloor":
          dptLfloor
        of "rfloor":
          dptRfloor
        of "lang":
          dptLang
        of "rang":
          dptRang
        of "loz":
          dptLoz
        of "spades":
          dptSpades
        of "clubs":
          dptClubs
        of "hearts":
          dptHearts
        of "diams":
          dptDiams
        of "OElig":
          dptOElig
        of "oelig":
          dptOelig1
        of "Scaron":
          dptScaron
        of "scaron":
          dptScaron1
        of "Yumlaut":
          dptYumlaut1
        of "circ":
          dptCirc
        of "tilde":
          dptTilde
        of "ensp":
          dptEnsp
        of "emsp":
          dptEmsp
        of "thinsp":
          dptThinsp
        of "zwnj":
          dptZwnj
        of "zwj":
          dptZwj
        of "lrm":
          dptLrm
        of "rlm":
          dptRlm
        of "ndash":
          dptNdash
        of "mdash":
          dptMdash
        of "lsquo":
          dptLsquo
        of "rsquo":
          dptRsquo
        of "sbquo":
          dptSbquo
        of "ldquo":
          dptLdquo
        of "rdquo":
          dptRdquo
        of "bdquo":
          dptBdquo
        of "dagger":
          dptDagger
        of "Dagger":
          dptDagger1
        of "permil":
          dptPermil
        of "lsaquo":
          dptLsaquo
        of "rsaquo":
          dptRsaquo
        of "euro":
          dptEuro
        of "tm":
          dptTm
        of "hruler":
          dptHruler
        else:
          dptLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocParaTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
      of "programlisting":
        ## 617:12:xml_to_types.nim
        var tmp: ListingType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptProgramlisting, listingType: tmp))
      of "indexentry":
        ## 617:12:xml_to_types.nim
        var tmp: DocIndexEntryType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptIndexentry, docIndexEntryType: tmp))
      of "orderedlist", "itemizedlist":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "orderedlist":
          dptOrderedlist
        of "itemizedlist":
          dptItemizedlist
        else:
          dptOrderedlist
        var tmp: DocListType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocParaTypeBody(kind: kind)
        tmp2.docListType = tmp
        add(target.xsdChoice, tmp2)
      of "simplesect":
        ## 617:12:xml_to_types.nim
        var tmp: DocSimpleSectType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptSimplesect, docSimpleSectType: tmp))
      of "title":
        ## 617:12:xml_to_types.nim
        var tmp: DocTitleType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocParaTypeBody(kind: dptTitle, docTitleType: tmp))
      of "variablelist":
        ## 617:12:xml_to_types.nim
        var tmp: DocVariableListType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptVariablelist, docVariableListType: tmp))
      of "table":
        ## 617:12:xml_to_types.nim
        var tmp: DocTableType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocParaTypeBody(kind: dptTable, docTableType: tmp))
      of "heading":
        ## 617:12:xml_to_types.nim
        var tmp: DocHeadingType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptHeading, docHeadingType: tmp))
      of "toclist":
        ## 617:12:xml_to_types.nim
        var tmp: DocTocListType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptToclist, docTocListType: tmp))
      of "language":
        ## 617:12:xml_to_types.nim
        var tmp: DocLanguageType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptLanguage, docLanguageType: tmp))
      of "parameterlist":
        ## 617:12:xml_to_types.nim
        var tmp: DocParamListType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptParameterlist, docParamListType: tmp))
      of "xrefsect":
        ## 617:12:xml_to_types.nim
        var tmp: DocXRefSectType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptXrefsect, docXRefSectType: tmp))
      of "copydoc":
        ## 617:12:xml_to_types.nim
        var tmp: DocCopyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptCopydoc, docCopyType: tmp))
      of "blockquote":
        ## 617:12:xml_to_types.nim
        var tmp: DocBlockQuoteType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptBlockquote, docBlockQuoteType: tmp))
      of "parblock":
        ## 617:12:xml_to_types.nim
        var tmp: DocParBlockType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocParaTypeBody(kind: dptParblock, docParBlockType: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocMarkupType): int =
  item.xsdChoice.len


iterator items*(item: DocMarkupType): DocMarkupTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocMarkupType; idx: int): DocMarkupTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocMarkupType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocMarkupTypeBody(kind: dmtMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocMarkupTypeBody(kind: dmtUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins",
         "preformatted":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          dmtBold
        of "s":
          dmtS
        of "strike":
          dmtStrike
        of "underline":
          dmtUnderline
        of "emphasis":
          dmtEmphasis
        of "computeroutput":
          dmtComputeroutput
        of "subscript":
          dmtSubscript
        of "superscript":
          dmtSuperscript
        of "center":
          dmtCenter
        of "small":
          dmtSmall
        of "del":
          dmtDel
        of "ins":
          dmtIns
        of "preformatted":
          dmtPreformatted
        else:
          dmtBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocMarkupTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly", "verbatim":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          dmtManonly
        of "xmlonly":
          dmtXmlonly
        of "rtfonly":
          dmtRtfonly
        of "latexonly":
          dmtLatexonly
        of "docbookonly":
          dmtDocbookonly
        of "verbatim":
          dmtVerbatim
        else:
          dmtManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocMarkupTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml", "dotfile", "mscfile", "diafile":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          dmtImage
        of "dot":
          dmtDot
        of "msc":
          dmtMsc
        of "plantuml":
          dmtPlantuml
        of "dotfile":
          dmtDotfile
        of "mscfile":
          dmtMscfile
        of "diafile":
          dmtDiafile
        else:
          dmtImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocMarkupTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm",
         "hruler":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          dmtLinebreak
        of "nonbreakablespace":
          dmtNonbreakablespace
        of "iexcl":
          dmtIexcl
        of "cent":
          dmtCent
        of "pound":
          dmtPound
        of "curren":
          dmtCurren
        of "yen":
          dmtYen
        of "brvbar":
          dmtBrvbar
        of "sect":
          dmtSect
        of "umlaut":
          dmtUmlaut
        of "copy":
          dmtCopy
        of "ordf":
          dmtOrdf
        of "laquo":
          dmtLaquo
        of "not":
          dmtNot
        of "shy":
          dmtShy
        of "registered":
          dmtRegistered
        of "macr":
          dmtMacr
        of "deg":
          dmtDeg
        of "plusmn":
          dmtPlusmn
        of "sup2":
          dmtSup2
        of "sup3":
          dmtSup3
        of "acute":
          dmtAcute
        of "micro":
          dmtMicro
        of "para":
          dmtPara
        of "middot":
          dmtMiddot
        of "cedil":
          dmtCedil
        of "sup1":
          dmtSup1
        of "ordm":
          dmtOrdm
        of "raquo":
          dmtRaquo
        of "frac14":
          dmtFrac14
        of "frac12":
          dmtFrac12
        of "frac34":
          dmtFrac34
        of "iquest":
          dmtIquest
        of "Agrave":
          dmtAgrave
        of "Aacute":
          dmtAacute
        of "Acirc":
          dmtAcirc
        of "Atilde":
          dmtAtilde
        of "Aumlaut":
          dmtAumlaut
        of "Aring":
          dmtAring
        of "AElig":
          dmtAElig
        of "Ccedil":
          dmtCcedil
        of "Egrave":
          dmtEgrave
        of "Eacute":
          dmtEacute
        of "Ecirc":
          dmtEcirc
        of "Eumlaut":
          dmtEumlaut
        of "Igrave":
          dmtIgrave
        of "Iacute":
          dmtIacute
        of "Icirc":
          dmtIcirc
        of "Iumlaut":
          dmtIumlaut
        of "ETH":
          dmtETH
        of "Ntilde":
          dmtNtilde
        of "Ograve":
          dmtOgrave
        of "Oacute":
          dmtOacute
        of "Ocirc":
          dmtOcirc
        of "Otilde":
          dmtOtilde
        of "Oumlaut":
          dmtOumlaut
        of "times":
          dmtTimes
        of "Oslash":
          dmtOslash
        of "Ugrave":
          dmtUgrave
        of "Uacute":
          dmtUacute
        of "Ucirc":
          dmtUcirc
        of "Uumlaut":
          dmtUumlaut
        of "Yacute":
          dmtYacute
        of "THORN":
          dmtTHORN
        of "szlig":
          dmtSzlig
        of "agrave":
          dmtAgrave1
        of "aacute":
          dmtAacute1
        of "acirc":
          dmtAcirc1
        of "atilde":
          dmtAtilde1
        of "aumlaut":
          dmtAumlaut1
        of "aring":
          dmtAring1
        of "aelig":
          dmtAelig1
        of "ccedil":
          dmtCcedil1
        of "egrave":
          dmtEgrave1
        of "eacute":
          dmtEacute1
        of "ecirc":
          dmtEcirc1
        of "eumlaut":
          dmtEumlaut1
        of "igrave":
          dmtIgrave1
        of "iacute":
          dmtIacute1
        of "icirc":
          dmtIcirc1
        of "iumlaut":
          dmtIumlaut1
        of "eth":
          dmtEth1
        of "ntilde":
          dmtNtilde1
        of "ograve":
          dmtOgrave1
        of "oacute":
          dmtOacute1
        of "ocirc":
          dmtOcirc1
        of "otilde":
          dmtOtilde1
        of "oumlaut":
          dmtOumlaut1
        of "divide":
          dmtDivide
        of "oslash":
          dmtOslash1
        of "ugrave":
          dmtUgrave1
        of "uacute":
          dmtUacute1
        of "ucirc":
          dmtUcirc1
        of "uumlaut":
          dmtUumlaut1
        of "yacute":
          dmtYacute1
        of "thorn":
          dmtThorn1
        of "yumlaut":
          dmtYumlaut
        of "fnof":
          dmtFnof
        of "Alpha":
          dmtAlpha
        of "Beta":
          dmtBeta
        of "Gamma":
          dmtGamma
        of "Delta":
          dmtDelta
        of "Epsilon":
          dmtEpsilon
        of "Zeta":
          dmtZeta
        of "Eta":
          dmtEta
        of "Theta":
          dmtTheta
        of "Iota":
          dmtIota
        of "Kappa":
          dmtKappa
        of "Lambda":
          dmtLambda
        of "Mu":
          dmtMu
        of "Nu":
          dmtNu
        of "Xi":
          dmtXi
        of "Omicron":
          dmtOmicron
        of "Pi":
          dmtPi
        of "Rho":
          dmtRho
        of "Sigma":
          dmtSigma
        of "Tau":
          dmtTau
        of "Upsilon":
          dmtUpsilon
        of "Phi":
          dmtPhi
        of "Chi":
          dmtChi
        of "Psi":
          dmtPsi
        of "Omega":
          dmtOmega
        of "alpha":
          dmtAlpha1
        of "beta":
          dmtBeta1
        of "gamma":
          dmtGamma1
        of "delta":
          dmtDelta1
        of "epsilon":
          dmtEpsilon1
        of "zeta":
          dmtZeta1
        of "eta":
          dmtEta1
        of "theta":
          dmtTheta1
        of "iota":
          dmtIota1
        of "kappa":
          dmtKappa1
        of "lambda":
          dmtLambda1
        of "mu":
          dmtMu1
        of "nu":
          dmtNu1
        of "xi":
          dmtXi1
        of "omicron":
          dmtOmicron1
        of "pi":
          dmtPi1
        of "rho":
          dmtRho1
        of "sigmaf":
          dmtSigmaf
        of "sigma":
          dmtSigma1
        of "tau":
          dmtTau1
        of "upsilon":
          dmtUpsilon1
        of "phi":
          dmtPhi1
        of "chi":
          dmtChi1
        of "psi":
          dmtPsi1
        of "omega":
          dmtOmega1
        of "thetasym":
          dmtThetasym
        of "upsih":
          dmtUpsih
        of "piv":
          dmtPiv
        of "bull":
          dmtBull
        of "hellip":
          dmtHellip
        of "prime":
          dmtPrime
        of "Prime":
          dmtPrime1
        of "oline":
          dmtOline
        of "frasl":
          dmtFrasl
        of "weierp":
          dmtWeierp
        of "imaginary":
          dmtImaginary
        of "real":
          dmtReal
        of "trademark":
          dmtTrademark
        of "alefsym":
          dmtAlefsym
        of "larr":
          dmtLarr
        of "uarr":
          dmtUarr
        of "rarr":
          dmtRarr
        of "darr":
          dmtDarr
        of "harr":
          dmtHarr
        of "crarr":
          dmtCrarr
        of "lArr":
          dmtLArr1
        of "uArr":
          dmtUArr1
        of "rArr":
          dmtRArr1
        of "dArr":
          dmtDArr1
        of "hArr":
          dmtHArr1
        of "forall":
          dmtForall
        of "part":
          dmtPart
        of "exist":
          dmtExist
        of "empty":
          dmtEmpty
        of "nabla":
          dmtNabla
        of "isin":
          dmtIsin
        of "notin":
          dmtNotin
        of "ni":
          dmtNi
        of "prod":
          dmtProd
        of "sum":
          dmtSum
        of "minus":
          dmtMinus
        of "lowast":
          dmtLowast
        of "radic":
          dmtRadic
        of "prop":
          dmtProp
        of "infin":
          dmtInfin
        of "ang":
          dmtAng
        of "and":
          dmtAnd
        of "or":
          dmtOr
        of "cap":
          dmtCap
        of "cup":
          dmtCup
        of "int":
          dmtInt
        of "there4":
          dmtThere4
        of "sim":
          dmtSim
        of "cong":
          dmtCong
        of "asymp":
          dmtAsymp
        of "ne":
          dmtNe
        of "equiv":
          dmtEquiv
        of "le":
          dmtLe
        of "ge":
          dmtGe
        of "sub":
          dmtSub
        of "sup":
          dmtSup
        of "nsub":
          dmtNsub
        of "sube":
          dmtSube
        of "supe":
          dmtSupe
        of "oplus":
          dmtOplus
        of "otimes":
          dmtOtimes
        of "perp":
          dmtPerp
        of "sdot":
          dmtSdot
        of "lceil":
          dmtLceil
        of "rceil":
          dmtRceil
        of "lfloor":
          dmtLfloor
        of "rfloor":
          dmtRfloor
        of "lang":
          dmtLang
        of "rang":
          dmtRang
        of "loz":
          dmtLoz
        of "spades":
          dmtSpades
        of "clubs":
          dmtClubs
        of "hearts":
          dmtHearts
        of "diams":
          dmtDiams
        of "OElig":
          dmtOElig
        of "oelig":
          dmtOelig1
        of "Scaron":
          dmtScaron
        of "scaron":
          dmtScaron1
        of "Yumlaut":
          dmtYumlaut1
        of "circ":
          dmtCirc
        of "tilde":
          dmtTilde
        of "ensp":
          dmtEnsp
        of "emsp":
          dmtEmsp
        of "thinsp":
          dmtThinsp
        of "zwnj":
          dmtZwnj
        of "zwj":
          dmtZwj
        of "lrm":
          dmtLrm
        of "rlm":
          dmtRlm
        of "ndash":
          dmtNdash
        of "mdash":
          dmtMdash
        of "lsquo":
          dmtLsquo
        of "rsquo":
          dmtRsquo
        of "sbquo":
          dmtSbquo
        of "ldquo":
          dmtLdquo
        of "rdquo":
          dmtRdquo
        of "bdquo":
          dmtBdquo
        of "dagger":
          dmtDagger
        of "Dagger":
          dmtDagger1
        of "permil":
          dmtPermil
        of "lsaquo":
          dmtLsaquo
        of "rsaquo":
          dmtRsaquo
        of "euro":
          dmtEuro
        of "tm":
          dmtTm
        of "hruler":
          dmtHruler
        else:
          dmtLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocMarkupTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
      of "programlisting":
        ## 617:12:xml_to_types.nim
        var tmp: ListingType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtProgramlisting, listingType: tmp))
      of "indexentry":
        ## 617:12:xml_to_types.nim
        var tmp: DocIndexEntryType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtIndexentry, docIndexEntryType: tmp))
      of "orderedlist", "itemizedlist":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "orderedlist":
          dmtOrderedlist
        of "itemizedlist":
          dmtItemizedlist
        else:
          dmtOrderedlist
        var tmp: DocListType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocMarkupTypeBody(kind: kind)
        tmp2.docListType = tmp
        add(target.xsdChoice, tmp2)
      of "simplesect":
        ## 617:12:xml_to_types.nim
        var tmp: DocSimpleSectType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtSimplesect, docSimpleSectType: tmp))
      of "title":
        ## 617:12:xml_to_types.nim
        var tmp: DocTitleType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtTitle, docTitleType: tmp))
      of "variablelist":
        ## 617:12:xml_to_types.nim
        var tmp: DocVariableListType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtVariablelist, docVariableListType: tmp))
      of "table":
        ## 617:12:xml_to_types.nim
        var tmp: DocTableType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtTable, docTableType: tmp))
      of "heading":
        ## 617:12:xml_to_types.nim
        var tmp: DocHeadingType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtHeading, docHeadingType: tmp))
      of "toclist":
        ## 617:12:xml_to_types.nim
        var tmp: DocTocListType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtToclist, docTocListType: tmp))
      of "language":
        ## 617:12:xml_to_types.nim
        var tmp: DocLanguageType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtLanguage, docLanguageType: tmp))
      of "parameterlist":
        ## 617:12:xml_to_types.nim
        var tmp: DocParamListType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtParameterlist, docParamListType: tmp))
      of "xrefsect":
        ## 617:12:xml_to_types.nim
        var tmp: DocXRefSectType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtXrefsect, docXRefSectType: tmp))
      of "copydoc":
        ## 617:12:xml_to_types.nim
        var tmp: DocCopyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtCopydoc, docCopyType: tmp))
      of "blockquote":
        ## 617:12:xml_to_types.nim
        var tmp: DocBlockQuoteType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtBlockquote, docBlockQuoteType: tmp))
      of "parblock":
        ## 617:12:xml_to_types.nim
        var tmp: DocParBlockType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocMarkupTypeBody(kind: dmtParblock, docParBlockType: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocURLLink): int =
  item.xsdChoice.len


iterator items*(item: DocURLLink): DocURLLinkBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocURLLink; idx: int): DocURLLinkBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocURLLink; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "url":
        loadXml(parser, target.url, "url")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocURLLinkBody(kind: dulMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocURLLinkBody(kind: dulUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          dulBold
        of "s":
          dulS
        of "strike":
          dulStrike
        of "underline":
          dulUnderline
        of "emphasis":
          dulEmphasis
        of "computeroutput":
          dulComputeroutput
        of "subscript":
          dulSubscript
        of "superscript":
          dulSuperscript
        of "center":
          dulCenter
        of "small":
          dulSmall
        of "del":
          dulDel
        of "ins":
          dulIns
        else:
          dulBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocURLLinkBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocURLLinkBody(kind: dulHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          dulManonly
        of "xmlonly":
          dulXmlonly
        of "rtfonly":
          dulRtfonly
        of "latexonly":
          dulLatexonly
        of "docbookonly":
          dulDocbookonly
        else:
          dulManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocURLLinkBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          dulImage
        of "dot":
          dulDot
        of "msc":
          dulMsc
        of "plantuml":
          dulPlantuml
        else:
          dulImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocURLLinkBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocURLLinkBody(kind: dulAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocURLLinkBody(kind: dulFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocURLLinkBody(kind: dulRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocURLLinkBody(kind: dulEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          dulLinebreak
        of "nonbreakablespace":
          dulNonbreakablespace
        of "iexcl":
          dulIexcl
        of "cent":
          dulCent
        of "pound":
          dulPound
        of "curren":
          dulCurren
        of "yen":
          dulYen
        of "brvbar":
          dulBrvbar
        of "sect":
          dulSect
        of "umlaut":
          dulUmlaut
        of "copy":
          dulCopy
        of "ordf":
          dulOrdf
        of "laquo":
          dulLaquo
        of "not":
          dulNot
        of "shy":
          dulShy
        of "registered":
          dulRegistered
        of "macr":
          dulMacr
        of "deg":
          dulDeg
        of "plusmn":
          dulPlusmn
        of "sup2":
          dulSup2
        of "sup3":
          dulSup3
        of "acute":
          dulAcute
        of "micro":
          dulMicro
        of "para":
          dulPara
        of "middot":
          dulMiddot
        of "cedil":
          dulCedil
        of "sup1":
          dulSup1
        of "ordm":
          dulOrdm
        of "raquo":
          dulRaquo
        of "frac14":
          dulFrac14
        of "frac12":
          dulFrac12
        of "frac34":
          dulFrac34
        of "iquest":
          dulIquest
        of "Agrave":
          dulAgrave
        of "Aacute":
          dulAacute
        of "Acirc":
          dulAcirc
        of "Atilde":
          dulAtilde
        of "Aumlaut":
          dulAumlaut
        of "Aring":
          dulAring
        of "AElig":
          dulAElig
        of "Ccedil":
          dulCcedil
        of "Egrave":
          dulEgrave
        of "Eacute":
          dulEacute
        of "Ecirc":
          dulEcirc
        of "Eumlaut":
          dulEumlaut
        of "Igrave":
          dulIgrave
        of "Iacute":
          dulIacute
        of "Icirc":
          dulIcirc
        of "Iumlaut":
          dulIumlaut
        of "ETH":
          dulETH
        of "Ntilde":
          dulNtilde
        of "Ograve":
          dulOgrave
        of "Oacute":
          dulOacute
        of "Ocirc":
          dulOcirc
        of "Otilde":
          dulOtilde
        of "Oumlaut":
          dulOumlaut
        of "times":
          dulTimes
        of "Oslash":
          dulOslash
        of "Ugrave":
          dulUgrave
        of "Uacute":
          dulUacute
        of "Ucirc":
          dulUcirc
        of "Uumlaut":
          dulUumlaut
        of "Yacute":
          dulYacute
        of "THORN":
          dulTHORN
        of "szlig":
          dulSzlig
        of "agrave":
          dulAgrave1
        of "aacute":
          dulAacute1
        of "acirc":
          dulAcirc1
        of "atilde":
          dulAtilde1
        of "aumlaut":
          dulAumlaut1
        of "aring":
          dulAring1
        of "aelig":
          dulAelig1
        of "ccedil":
          dulCcedil1
        of "egrave":
          dulEgrave1
        of "eacute":
          dulEacute1
        of "ecirc":
          dulEcirc1
        of "eumlaut":
          dulEumlaut1
        of "igrave":
          dulIgrave1
        of "iacute":
          dulIacute1
        of "icirc":
          dulIcirc1
        of "iumlaut":
          dulIumlaut1
        of "eth":
          dulEth1
        of "ntilde":
          dulNtilde1
        of "ograve":
          dulOgrave1
        of "oacute":
          dulOacute1
        of "ocirc":
          dulOcirc1
        of "otilde":
          dulOtilde1
        of "oumlaut":
          dulOumlaut1
        of "divide":
          dulDivide
        of "oslash":
          dulOslash1
        of "ugrave":
          dulUgrave1
        of "uacute":
          dulUacute1
        of "ucirc":
          dulUcirc1
        of "uumlaut":
          dulUumlaut1
        of "yacute":
          dulYacute1
        of "thorn":
          dulThorn1
        of "yumlaut":
          dulYumlaut
        of "fnof":
          dulFnof
        of "Alpha":
          dulAlpha
        of "Beta":
          dulBeta
        of "Gamma":
          dulGamma
        of "Delta":
          dulDelta
        of "Epsilon":
          dulEpsilon
        of "Zeta":
          dulZeta
        of "Eta":
          dulEta
        of "Theta":
          dulTheta
        of "Iota":
          dulIota
        of "Kappa":
          dulKappa
        of "Lambda":
          dulLambda
        of "Mu":
          dulMu
        of "Nu":
          dulNu
        of "Xi":
          dulXi
        of "Omicron":
          dulOmicron
        of "Pi":
          dulPi
        of "Rho":
          dulRho
        of "Sigma":
          dulSigma
        of "Tau":
          dulTau
        of "Upsilon":
          dulUpsilon
        of "Phi":
          dulPhi
        of "Chi":
          dulChi
        of "Psi":
          dulPsi
        of "Omega":
          dulOmega
        of "alpha":
          dulAlpha1
        of "beta":
          dulBeta1
        of "gamma":
          dulGamma1
        of "delta":
          dulDelta1
        of "epsilon":
          dulEpsilon1
        of "zeta":
          dulZeta1
        of "eta":
          dulEta1
        of "theta":
          dulTheta1
        of "iota":
          dulIota1
        of "kappa":
          dulKappa1
        of "lambda":
          dulLambda1
        of "mu":
          dulMu1
        of "nu":
          dulNu1
        of "xi":
          dulXi1
        of "omicron":
          dulOmicron1
        of "pi":
          dulPi1
        of "rho":
          dulRho1
        of "sigmaf":
          dulSigmaf
        of "sigma":
          dulSigma1
        of "tau":
          dulTau1
        of "upsilon":
          dulUpsilon1
        of "phi":
          dulPhi1
        of "chi":
          dulChi1
        of "psi":
          dulPsi1
        of "omega":
          dulOmega1
        of "thetasym":
          dulThetasym
        of "upsih":
          dulUpsih
        of "piv":
          dulPiv
        of "bull":
          dulBull
        of "hellip":
          dulHellip
        of "prime":
          dulPrime
        of "Prime":
          dulPrime1
        of "oline":
          dulOline
        of "frasl":
          dulFrasl
        of "weierp":
          dulWeierp
        of "imaginary":
          dulImaginary
        of "real":
          dulReal
        of "trademark":
          dulTrademark
        of "alefsym":
          dulAlefsym
        of "larr":
          dulLarr
        of "uarr":
          dulUarr
        of "rarr":
          dulRarr
        of "darr":
          dulDarr
        of "harr":
          dulHarr
        of "crarr":
          dulCrarr
        of "lArr":
          dulLArr1
        of "uArr":
          dulUArr1
        of "rArr":
          dulRArr1
        of "dArr":
          dulDArr1
        of "hArr":
          dulHArr1
        of "forall":
          dulForall
        of "part":
          dulPart
        of "exist":
          dulExist
        of "empty":
          dulEmpty
        of "nabla":
          dulNabla
        of "isin":
          dulIsin
        of "notin":
          dulNotin
        of "ni":
          dulNi
        of "prod":
          dulProd
        of "sum":
          dulSum
        of "minus":
          dulMinus
        of "lowast":
          dulLowast
        of "radic":
          dulRadic
        of "prop":
          dulProp
        of "infin":
          dulInfin
        of "ang":
          dulAng
        of "and":
          dulAnd
        of "or":
          dulOr
        of "cap":
          dulCap
        of "cup":
          dulCup
        of "int":
          dulInt
        of "there4":
          dulThere4
        of "sim":
          dulSim
        of "cong":
          dulCong
        of "asymp":
          dulAsymp
        of "ne":
          dulNe
        of "equiv":
          dulEquiv
        of "le":
          dulLe
        of "ge":
          dulGe
        of "sub":
          dulSub
        of "sup":
          dulSup
        of "nsub":
          dulNsub
        of "sube":
          dulSube
        of "supe":
          dulSupe
        of "oplus":
          dulOplus
        of "otimes":
          dulOtimes
        of "perp":
          dulPerp
        of "sdot":
          dulSdot
        of "lceil":
          dulLceil
        of "rceil":
          dulRceil
        of "lfloor":
          dulLfloor
        of "rfloor":
          dulRfloor
        of "lang":
          dulLang
        of "rang":
          dulRang
        of "loz":
          dulLoz
        of "spades":
          dulSpades
        of "clubs":
          dulClubs
        of "hearts":
          dulHearts
        of "diams":
          dulDiams
        of "OElig":
          dulOElig
        of "oelig":
          dulOelig1
        of "Scaron":
          dulScaron
        of "scaron":
          dulScaron1
        of "Yumlaut":
          dulYumlaut1
        of "circ":
          dulCirc
        of "tilde":
          dulTilde
        of "ensp":
          dulEnsp
        of "emsp":
          dulEmsp
        of "thinsp":
          dulThinsp
        of "zwnj":
          dulZwnj
        of "zwj":
          dulZwj
        of "lrm":
          dulLrm
        of "rlm":
          dulRlm
        of "ndash":
          dulNdash
        of "mdash":
          dulMdash
        of "lsquo":
          dulLsquo
        of "rsquo":
          dulRsquo
        of "sbquo":
          dulSbquo
        of "ldquo":
          dulLdquo
        of "rdquo":
          dulRdquo
        of "bdquo":
          dulBdquo
        of "dagger":
          dulDagger
        of "Dagger":
          dulDagger1
        of "permil":
          dulPermil
        of "lsaquo":
          dulLsaquo
        of "rsaquo":
          dulRsaquo
        of "euro":
          dulEuro
        of "tm":
          dulTm
        else:
          dulLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocURLLinkBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocAnchorType): int =
  item.xsdChoice.len


iterator items*(item: DocAnchorType): DocAnchorTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocAnchorType; idx: int): DocAnchorTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocAnchorType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocAnchorTypeBody(kind: datMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocFormulaType): int =
  item.xsdChoice.len


iterator items*(item: DocFormulaType): DocFormulaTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocFormulaType; idx: int): DocFormulaTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocFormulaType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocFormulaTypeBody(kind: dftMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var DocIndexEntryType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "primaryie":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.primaryie, "primaryie")
      of "secondaryie":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.secondaryie, "secondaryie")
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


proc loadXml*(parser: var HXmlParser; target: var DocListType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "listitem":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.listitem, "listitem")
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


proc loadXml*(parser: var HXmlParser; target: var DocListItemType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.para, "para")
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


proc loadXml*(parser: var HXmlParser; target: var DocSimpleSectType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
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
      of "title":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.title, "title")
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


proc loadXml*(parser: var HXmlParser; target: var DocVarListEntryType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "term":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.term, "term")
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


proc loadXml*(parser: var HXmlParser; target: var DocVariableListType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "varlistentry":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.varlistentry, "varlistentry")
      of "listitem":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.listitem, "listitem")
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


proc len*(item: DocRefTextType): int =
  item.xsdChoice.len


iterator items*(item: DocRefTextType): DocRefTextTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocRefTextType; idx: int): DocRefTextTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocRefTextType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "refid":
        loadXml(parser, target.refid, "refid")
      of "kindref":
        loadXml(parser, target.kindref, "kindref")
      of "external":
        loadXml(parser, target.external, "external")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocRefTextTypeBody(kind: drttMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocRefTextTypeBody(kind: drttUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          drttBold
        of "s":
          drttS
        of "strike":
          drttStrike
        of "underline":
          drttUnderline
        of "emphasis":
          drttEmphasis
        of "computeroutput":
          drttComputeroutput
        of "subscript":
          drttSubscript
        of "superscript":
          drttSuperscript
        of "center":
          drttCenter
        of "small":
          drttSmall
        of "del":
          drttDel
        of "ins":
          drttIns
        else:
          drttBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocRefTextTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocRefTextTypeBody(kind: drttHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          drttManonly
        of "xmlonly":
          drttXmlonly
        of "rtfonly":
          drttRtfonly
        of "latexonly":
          drttLatexonly
        of "docbookonly":
          drttDocbookonly
        else:
          drttManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocRefTextTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          drttImage
        of "dot":
          drttDot
        of "msc":
          drttMsc
        of "plantuml":
          drttPlantuml
        else:
          drttImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocRefTextTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocRefTextTypeBody(kind: drttAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocRefTextTypeBody(kind: drttFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocRefTextTypeBody(kind: drttRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocRefTextTypeBody(kind: drttEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          drttLinebreak
        of "nonbreakablespace":
          drttNonbreakablespace
        of "iexcl":
          drttIexcl
        of "cent":
          drttCent
        of "pound":
          drttPound
        of "curren":
          drttCurren
        of "yen":
          drttYen
        of "brvbar":
          drttBrvbar
        of "sect":
          drttSect
        of "umlaut":
          drttUmlaut
        of "copy":
          drttCopy
        of "ordf":
          drttOrdf
        of "laquo":
          drttLaquo
        of "not":
          drttNot
        of "shy":
          drttShy
        of "registered":
          drttRegistered
        of "macr":
          drttMacr
        of "deg":
          drttDeg
        of "plusmn":
          drttPlusmn
        of "sup2":
          drttSup2
        of "sup3":
          drttSup3
        of "acute":
          drttAcute
        of "micro":
          drttMicro
        of "para":
          drttPara
        of "middot":
          drttMiddot
        of "cedil":
          drttCedil
        of "sup1":
          drttSup1
        of "ordm":
          drttOrdm
        of "raquo":
          drttRaquo
        of "frac14":
          drttFrac14
        of "frac12":
          drttFrac12
        of "frac34":
          drttFrac34
        of "iquest":
          drttIquest
        of "Agrave":
          drttAgrave
        of "Aacute":
          drttAacute
        of "Acirc":
          drttAcirc
        of "Atilde":
          drttAtilde
        of "Aumlaut":
          drttAumlaut
        of "Aring":
          drttAring
        of "AElig":
          drttAElig
        of "Ccedil":
          drttCcedil
        of "Egrave":
          drttEgrave
        of "Eacute":
          drttEacute
        of "Ecirc":
          drttEcirc
        of "Eumlaut":
          drttEumlaut
        of "Igrave":
          drttIgrave
        of "Iacute":
          drttIacute
        of "Icirc":
          drttIcirc
        of "Iumlaut":
          drttIumlaut
        of "ETH":
          drttETH
        of "Ntilde":
          drttNtilde
        of "Ograve":
          drttOgrave
        of "Oacute":
          drttOacute
        of "Ocirc":
          drttOcirc
        of "Otilde":
          drttOtilde
        of "Oumlaut":
          drttOumlaut
        of "times":
          drttTimes
        of "Oslash":
          drttOslash
        of "Ugrave":
          drttUgrave
        of "Uacute":
          drttUacute
        of "Ucirc":
          drttUcirc
        of "Uumlaut":
          drttUumlaut
        of "Yacute":
          drttYacute
        of "THORN":
          drttTHORN
        of "szlig":
          drttSzlig
        of "agrave":
          drttAgrave1
        of "aacute":
          drttAacute1
        of "acirc":
          drttAcirc1
        of "atilde":
          drttAtilde1
        of "aumlaut":
          drttAumlaut1
        of "aring":
          drttAring1
        of "aelig":
          drttAelig1
        of "ccedil":
          drttCcedil1
        of "egrave":
          drttEgrave1
        of "eacute":
          drttEacute1
        of "ecirc":
          drttEcirc1
        of "eumlaut":
          drttEumlaut1
        of "igrave":
          drttIgrave1
        of "iacute":
          drttIacute1
        of "icirc":
          drttIcirc1
        of "iumlaut":
          drttIumlaut1
        of "eth":
          drttEth1
        of "ntilde":
          drttNtilde1
        of "ograve":
          drttOgrave1
        of "oacute":
          drttOacute1
        of "ocirc":
          drttOcirc1
        of "otilde":
          drttOtilde1
        of "oumlaut":
          drttOumlaut1
        of "divide":
          drttDivide
        of "oslash":
          drttOslash1
        of "ugrave":
          drttUgrave1
        of "uacute":
          drttUacute1
        of "ucirc":
          drttUcirc1
        of "uumlaut":
          drttUumlaut1
        of "yacute":
          drttYacute1
        of "thorn":
          drttThorn1
        of "yumlaut":
          drttYumlaut
        of "fnof":
          drttFnof
        of "Alpha":
          drttAlpha
        of "Beta":
          drttBeta
        of "Gamma":
          drttGamma
        of "Delta":
          drttDelta
        of "Epsilon":
          drttEpsilon
        of "Zeta":
          drttZeta
        of "Eta":
          drttEta
        of "Theta":
          drttTheta
        of "Iota":
          drttIota
        of "Kappa":
          drttKappa
        of "Lambda":
          drttLambda
        of "Mu":
          drttMu
        of "Nu":
          drttNu
        of "Xi":
          drttXi
        of "Omicron":
          drttOmicron
        of "Pi":
          drttPi
        of "Rho":
          drttRho
        of "Sigma":
          drttSigma
        of "Tau":
          drttTau
        of "Upsilon":
          drttUpsilon
        of "Phi":
          drttPhi
        of "Chi":
          drttChi
        of "Psi":
          drttPsi
        of "Omega":
          drttOmega
        of "alpha":
          drttAlpha1
        of "beta":
          drttBeta1
        of "gamma":
          drttGamma1
        of "delta":
          drttDelta1
        of "epsilon":
          drttEpsilon1
        of "zeta":
          drttZeta1
        of "eta":
          drttEta1
        of "theta":
          drttTheta1
        of "iota":
          drttIota1
        of "kappa":
          drttKappa1
        of "lambda":
          drttLambda1
        of "mu":
          drttMu1
        of "nu":
          drttNu1
        of "xi":
          drttXi1
        of "omicron":
          drttOmicron1
        of "pi":
          drttPi1
        of "rho":
          drttRho1
        of "sigmaf":
          drttSigmaf
        of "sigma":
          drttSigma1
        of "tau":
          drttTau1
        of "upsilon":
          drttUpsilon1
        of "phi":
          drttPhi1
        of "chi":
          drttChi1
        of "psi":
          drttPsi1
        of "omega":
          drttOmega1
        of "thetasym":
          drttThetasym
        of "upsih":
          drttUpsih
        of "piv":
          drttPiv
        of "bull":
          drttBull
        of "hellip":
          drttHellip
        of "prime":
          drttPrime
        of "Prime":
          drttPrime1
        of "oline":
          drttOline
        of "frasl":
          drttFrasl
        of "weierp":
          drttWeierp
        of "imaginary":
          drttImaginary
        of "real":
          drttReal
        of "trademark":
          drttTrademark
        of "alefsym":
          drttAlefsym
        of "larr":
          drttLarr
        of "uarr":
          drttUarr
        of "rarr":
          drttRarr
        of "darr":
          drttDarr
        of "harr":
          drttHarr
        of "crarr":
          drttCrarr
        of "lArr":
          drttLArr1
        of "uArr":
          drttUArr1
        of "rArr":
          drttRArr1
        of "dArr":
          drttDArr1
        of "hArr":
          drttHArr1
        of "forall":
          drttForall
        of "part":
          drttPart
        of "exist":
          drttExist
        of "empty":
          drttEmpty
        of "nabla":
          drttNabla
        of "isin":
          drttIsin
        of "notin":
          drttNotin
        of "ni":
          drttNi
        of "prod":
          drttProd
        of "sum":
          drttSum
        of "minus":
          drttMinus
        of "lowast":
          drttLowast
        of "radic":
          drttRadic
        of "prop":
          drttProp
        of "infin":
          drttInfin
        of "ang":
          drttAng
        of "and":
          drttAnd
        of "or":
          drttOr
        of "cap":
          drttCap
        of "cup":
          drttCup
        of "int":
          drttInt
        of "there4":
          drttThere4
        of "sim":
          drttSim
        of "cong":
          drttCong
        of "asymp":
          drttAsymp
        of "ne":
          drttNe
        of "equiv":
          drttEquiv
        of "le":
          drttLe
        of "ge":
          drttGe
        of "sub":
          drttSub
        of "sup":
          drttSup
        of "nsub":
          drttNsub
        of "sube":
          drttSube
        of "supe":
          drttSupe
        of "oplus":
          drttOplus
        of "otimes":
          drttOtimes
        of "perp":
          drttPerp
        of "sdot":
          drttSdot
        of "lceil":
          drttLceil
        of "rceil":
          drttRceil
        of "lfloor":
          drttLfloor
        of "rfloor":
          drttRfloor
        of "lang":
          drttLang
        of "rang":
          drttRang
        of "loz":
          drttLoz
        of "spades":
          drttSpades
        of "clubs":
          drttClubs
        of "hearts":
          drttHearts
        of "diams":
          drttDiams
        of "OElig":
          drttOElig
        of "oelig":
          drttOelig1
        of "Scaron":
          drttScaron
        of "scaron":
          drttScaron1
        of "Yumlaut":
          drttYumlaut1
        of "circ":
          drttCirc
        of "tilde":
          drttTilde
        of "ensp":
          drttEnsp
        of "emsp":
          drttEmsp
        of "thinsp":
          drttThinsp
        of "zwnj":
          drttZwnj
        of "zwj":
          drttZwj
        of "lrm":
          drttLrm
        of "rlm":
          drttRlm
        of "ndash":
          drttNdash
        of "mdash":
          drttMdash
        of "lsquo":
          drttLsquo
        of "rsquo":
          drttRsquo
        of "sbquo":
          drttSbquo
        of "ldquo":
          drttLdquo
        of "rdquo":
          drttRdquo
        of "bdquo":
          drttBdquo
        of "dagger":
          drttDagger
        of "Dagger":
          drttDagger1
        of "permil":
          drttPermil
        of "lsaquo":
          drttLsaquo
        of "rsaquo":
          drttRsaquo
        of "euro":
          drttEuro
        of "tm":
          drttTm
        else:
          drttLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocRefTextTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var DocTableType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "rows":
        loadXml(parser, target.rows, "rows")
      of "cols":
        loadXml(parser, target.cols, "cols")
      of "width":
        loadXml(parser, target.width, "width")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "caption":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.caption, "caption")
      of "row":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.row, "row")
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


proc loadXml*(parser: var HXmlParser; target: var DocRowType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "entry":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.entry, "entry")
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


proc loadXml*(parser: var HXmlParser; target: var DocEntryType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "thead":
        loadXml(parser, target.thead, "thead")
      of "colspan":
        loadXml(parser, target.colspan, "colspan")
      of "rowspan":
        loadXml(parser, target.rowspan, "rowspan")
      of "align":
        loadXml(parser, target.align, "align")
      of "valign":
        loadXml(parser, target.valign, "valign")
      of "width":
        loadXml(parser, target.width, "width")
      of "class":
        loadXml(parser, target.class, "class")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.para, "para")
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


proc len*(item: DocCaptionType): int =
  item.xsdChoice.len


iterator items*(item: DocCaptionType): DocCaptionTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocCaptionType; idx: int): DocCaptionTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocCaptionType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocCaptionTypeBody(kind: dctMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocCaptionTypeBody(kind: dctUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          dctBold
        of "s":
          dctS
        of "strike":
          dctStrike
        of "underline":
          dctUnderline
        of "emphasis":
          dctEmphasis
        of "computeroutput":
          dctComputeroutput
        of "subscript":
          dctSubscript
        of "superscript":
          dctSuperscript
        of "center":
          dctCenter
        of "small":
          dctSmall
        of "del":
          dctDel
        of "ins":
          dctIns
        else:
          dctBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocCaptionTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocCaptionTypeBody(kind: dctHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          dctManonly
        of "xmlonly":
          dctXmlonly
        of "rtfonly":
          dctRtfonly
        of "latexonly":
          dctLatexonly
        of "docbookonly":
          dctDocbookonly
        else:
          dctManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocCaptionTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          dctImage
        of "dot":
          dctDot
        of "msc":
          dctMsc
        of "plantuml":
          dctPlantuml
        else:
          dctImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocCaptionTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocCaptionTypeBody(kind: dctAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocCaptionTypeBody(kind: dctFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocCaptionTypeBody(kind: dctRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocCaptionTypeBody(kind: dctEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          dctLinebreak
        of "nonbreakablespace":
          dctNonbreakablespace
        of "iexcl":
          dctIexcl
        of "cent":
          dctCent
        of "pound":
          dctPound
        of "curren":
          dctCurren
        of "yen":
          dctYen
        of "brvbar":
          dctBrvbar
        of "sect":
          dctSect
        of "umlaut":
          dctUmlaut
        of "copy":
          dctCopy
        of "ordf":
          dctOrdf
        of "laquo":
          dctLaquo
        of "not":
          dctNot
        of "shy":
          dctShy
        of "registered":
          dctRegistered
        of "macr":
          dctMacr
        of "deg":
          dctDeg
        of "plusmn":
          dctPlusmn
        of "sup2":
          dctSup2
        of "sup3":
          dctSup3
        of "acute":
          dctAcute
        of "micro":
          dctMicro
        of "para":
          dctPara
        of "middot":
          dctMiddot
        of "cedil":
          dctCedil
        of "sup1":
          dctSup1
        of "ordm":
          dctOrdm
        of "raquo":
          dctRaquo
        of "frac14":
          dctFrac14
        of "frac12":
          dctFrac12
        of "frac34":
          dctFrac34
        of "iquest":
          dctIquest
        of "Agrave":
          dctAgrave
        of "Aacute":
          dctAacute
        of "Acirc":
          dctAcirc
        of "Atilde":
          dctAtilde
        of "Aumlaut":
          dctAumlaut
        of "Aring":
          dctAring
        of "AElig":
          dctAElig
        of "Ccedil":
          dctCcedil
        of "Egrave":
          dctEgrave
        of "Eacute":
          dctEacute
        of "Ecirc":
          dctEcirc
        of "Eumlaut":
          dctEumlaut
        of "Igrave":
          dctIgrave
        of "Iacute":
          dctIacute
        of "Icirc":
          dctIcirc
        of "Iumlaut":
          dctIumlaut
        of "ETH":
          dctETH
        of "Ntilde":
          dctNtilde
        of "Ograve":
          dctOgrave
        of "Oacute":
          dctOacute
        of "Ocirc":
          dctOcirc
        of "Otilde":
          dctOtilde
        of "Oumlaut":
          dctOumlaut
        of "times":
          dctTimes
        of "Oslash":
          dctOslash
        of "Ugrave":
          dctUgrave
        of "Uacute":
          dctUacute
        of "Ucirc":
          dctUcirc
        of "Uumlaut":
          dctUumlaut
        of "Yacute":
          dctYacute
        of "THORN":
          dctTHORN
        of "szlig":
          dctSzlig
        of "agrave":
          dctAgrave1
        of "aacute":
          dctAacute1
        of "acirc":
          dctAcirc1
        of "atilde":
          dctAtilde1
        of "aumlaut":
          dctAumlaut1
        of "aring":
          dctAring1
        of "aelig":
          dctAelig1
        of "ccedil":
          dctCcedil1
        of "egrave":
          dctEgrave1
        of "eacute":
          dctEacute1
        of "ecirc":
          dctEcirc1
        of "eumlaut":
          dctEumlaut1
        of "igrave":
          dctIgrave1
        of "iacute":
          dctIacute1
        of "icirc":
          dctIcirc1
        of "iumlaut":
          dctIumlaut1
        of "eth":
          dctEth1
        of "ntilde":
          dctNtilde1
        of "ograve":
          dctOgrave1
        of "oacute":
          dctOacute1
        of "ocirc":
          dctOcirc1
        of "otilde":
          dctOtilde1
        of "oumlaut":
          dctOumlaut1
        of "divide":
          dctDivide
        of "oslash":
          dctOslash1
        of "ugrave":
          dctUgrave1
        of "uacute":
          dctUacute1
        of "ucirc":
          dctUcirc1
        of "uumlaut":
          dctUumlaut1
        of "yacute":
          dctYacute1
        of "thorn":
          dctThorn1
        of "yumlaut":
          dctYumlaut
        of "fnof":
          dctFnof
        of "Alpha":
          dctAlpha
        of "Beta":
          dctBeta
        of "Gamma":
          dctGamma
        of "Delta":
          dctDelta
        of "Epsilon":
          dctEpsilon
        of "Zeta":
          dctZeta
        of "Eta":
          dctEta
        of "Theta":
          dctTheta
        of "Iota":
          dctIota
        of "Kappa":
          dctKappa
        of "Lambda":
          dctLambda
        of "Mu":
          dctMu
        of "Nu":
          dctNu
        of "Xi":
          dctXi
        of "Omicron":
          dctOmicron
        of "Pi":
          dctPi
        of "Rho":
          dctRho
        of "Sigma":
          dctSigma
        of "Tau":
          dctTau
        of "Upsilon":
          dctUpsilon
        of "Phi":
          dctPhi
        of "Chi":
          dctChi
        of "Psi":
          dctPsi
        of "Omega":
          dctOmega
        of "alpha":
          dctAlpha1
        of "beta":
          dctBeta1
        of "gamma":
          dctGamma1
        of "delta":
          dctDelta1
        of "epsilon":
          dctEpsilon1
        of "zeta":
          dctZeta1
        of "eta":
          dctEta1
        of "theta":
          dctTheta1
        of "iota":
          dctIota1
        of "kappa":
          dctKappa1
        of "lambda":
          dctLambda1
        of "mu":
          dctMu1
        of "nu":
          dctNu1
        of "xi":
          dctXi1
        of "omicron":
          dctOmicron1
        of "pi":
          dctPi1
        of "rho":
          dctRho1
        of "sigmaf":
          dctSigmaf
        of "sigma":
          dctSigma1
        of "tau":
          dctTau1
        of "upsilon":
          dctUpsilon1
        of "phi":
          dctPhi1
        of "chi":
          dctChi1
        of "psi":
          dctPsi1
        of "omega":
          dctOmega1
        of "thetasym":
          dctThetasym
        of "upsih":
          dctUpsih
        of "piv":
          dctPiv
        of "bull":
          dctBull
        of "hellip":
          dctHellip
        of "prime":
          dctPrime
        of "Prime":
          dctPrime1
        of "oline":
          dctOline
        of "frasl":
          dctFrasl
        of "weierp":
          dctWeierp
        of "imaginary":
          dctImaginary
        of "real":
          dctReal
        of "trademark":
          dctTrademark
        of "alefsym":
          dctAlefsym
        of "larr":
          dctLarr
        of "uarr":
          dctUarr
        of "rarr":
          dctRarr
        of "darr":
          dctDarr
        of "harr":
          dctHarr
        of "crarr":
          dctCrarr
        of "lArr":
          dctLArr1
        of "uArr":
          dctUArr1
        of "rArr":
          dctRArr1
        of "dArr":
          dctDArr1
        of "hArr":
          dctHArr1
        of "forall":
          dctForall
        of "part":
          dctPart
        of "exist":
          dctExist
        of "empty":
          dctEmpty
        of "nabla":
          dctNabla
        of "isin":
          dctIsin
        of "notin":
          dctNotin
        of "ni":
          dctNi
        of "prod":
          dctProd
        of "sum":
          dctSum
        of "minus":
          dctMinus
        of "lowast":
          dctLowast
        of "radic":
          dctRadic
        of "prop":
          dctProp
        of "infin":
          dctInfin
        of "ang":
          dctAng
        of "and":
          dctAnd
        of "or":
          dctOr
        of "cap":
          dctCap
        of "cup":
          dctCup
        of "int":
          dctInt
        of "there4":
          dctThere4
        of "sim":
          dctSim
        of "cong":
          dctCong
        of "asymp":
          dctAsymp
        of "ne":
          dctNe
        of "equiv":
          dctEquiv
        of "le":
          dctLe
        of "ge":
          dctGe
        of "sub":
          dctSub
        of "sup":
          dctSup
        of "nsub":
          dctNsub
        of "sube":
          dctSube
        of "supe":
          dctSupe
        of "oplus":
          dctOplus
        of "otimes":
          dctOtimes
        of "perp":
          dctPerp
        of "sdot":
          dctSdot
        of "lceil":
          dctLceil
        of "rceil":
          dctRceil
        of "lfloor":
          dctLfloor
        of "rfloor":
          dctRfloor
        of "lang":
          dctLang
        of "rang":
          dctRang
        of "loz":
          dctLoz
        of "spades":
          dctSpades
        of "clubs":
          dctClubs
        of "hearts":
          dctHearts
        of "diams":
          dctDiams
        of "OElig":
          dctOElig
        of "oelig":
          dctOelig1
        of "Scaron":
          dctScaron
        of "scaron":
          dctScaron1
        of "Yumlaut":
          dctYumlaut1
        of "circ":
          dctCirc
        of "tilde":
          dctTilde
        of "ensp":
          dctEnsp
        of "emsp":
          dctEmsp
        of "thinsp":
          dctThinsp
        of "zwnj":
          dctZwnj
        of "zwj":
          dctZwj
        of "lrm":
          dctLrm
        of "rlm":
          dctRlm
        of "ndash":
          dctNdash
        of "mdash":
          dctMdash
        of "lsquo":
          dctLsquo
        of "rsquo":
          dctRsquo
        of "sbquo":
          dctSbquo
        of "ldquo":
          dctLdquo
        of "rdquo":
          dctRdquo
        of "bdquo":
          dctBdquo
        of "dagger":
          dctDagger
        of "Dagger":
          dctDagger1
        of "permil":
          dctPermil
        of "lsaquo":
          dctLsaquo
        of "rsaquo":
          dctRsaquo
        of "euro":
          dctEuro
        of "tm":
          dctTm
        else:
          dctLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocCaptionTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocHeadingType): int =
  item.xsdChoice.len


iterator items*(item: DocHeadingType): DocHeadingTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocHeadingType; idx: int): DocHeadingTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocHeadingType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "level":
        loadXml(parser, target.level, "level")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocHeadingTypeBody(kind: dhtMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocHeadingTypeBody(kind: dhtUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          dhtBold
        of "s":
          dhtS
        of "strike":
          dhtStrike
        of "underline":
          dhtUnderline
        of "emphasis":
          dhtEmphasis
        of "computeroutput":
          dhtComputeroutput
        of "subscript":
          dhtSubscript
        of "superscript":
          dhtSuperscript
        of "center":
          dhtCenter
        of "small":
          dhtSmall
        of "del":
          dhtDel
        of "ins":
          dhtIns
        else:
          dhtBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocHeadingTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocHeadingTypeBody(kind: dhtHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          dhtManonly
        of "xmlonly":
          dhtXmlonly
        of "rtfonly":
          dhtRtfonly
        of "latexonly":
          dhtLatexonly
        of "docbookonly":
          dhtDocbookonly
        else:
          dhtManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocHeadingTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          dhtImage
        of "dot":
          dhtDot
        of "msc":
          dhtMsc
        of "plantuml":
          dhtPlantuml
        else:
          dhtImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocHeadingTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocHeadingTypeBody(kind: dhtAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocHeadingTypeBody(kind: dhtFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocHeadingTypeBody(kind: dhtRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocHeadingTypeBody(kind: dhtEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          dhtLinebreak
        of "nonbreakablespace":
          dhtNonbreakablespace
        of "iexcl":
          dhtIexcl
        of "cent":
          dhtCent
        of "pound":
          dhtPound
        of "curren":
          dhtCurren
        of "yen":
          dhtYen
        of "brvbar":
          dhtBrvbar
        of "sect":
          dhtSect
        of "umlaut":
          dhtUmlaut
        of "copy":
          dhtCopy
        of "ordf":
          dhtOrdf
        of "laquo":
          dhtLaquo
        of "not":
          dhtNot
        of "shy":
          dhtShy
        of "registered":
          dhtRegistered
        of "macr":
          dhtMacr
        of "deg":
          dhtDeg
        of "plusmn":
          dhtPlusmn
        of "sup2":
          dhtSup2
        of "sup3":
          dhtSup3
        of "acute":
          dhtAcute
        of "micro":
          dhtMicro
        of "para":
          dhtPara
        of "middot":
          dhtMiddot
        of "cedil":
          dhtCedil
        of "sup1":
          dhtSup1
        of "ordm":
          dhtOrdm
        of "raquo":
          dhtRaquo
        of "frac14":
          dhtFrac14
        of "frac12":
          dhtFrac12
        of "frac34":
          dhtFrac34
        of "iquest":
          dhtIquest
        of "Agrave":
          dhtAgrave
        of "Aacute":
          dhtAacute
        of "Acirc":
          dhtAcirc
        of "Atilde":
          dhtAtilde
        of "Aumlaut":
          dhtAumlaut
        of "Aring":
          dhtAring
        of "AElig":
          dhtAElig
        of "Ccedil":
          dhtCcedil
        of "Egrave":
          dhtEgrave
        of "Eacute":
          dhtEacute
        of "Ecirc":
          dhtEcirc
        of "Eumlaut":
          dhtEumlaut
        of "Igrave":
          dhtIgrave
        of "Iacute":
          dhtIacute
        of "Icirc":
          dhtIcirc
        of "Iumlaut":
          dhtIumlaut
        of "ETH":
          dhtETH
        of "Ntilde":
          dhtNtilde
        of "Ograve":
          dhtOgrave
        of "Oacute":
          dhtOacute
        of "Ocirc":
          dhtOcirc
        of "Otilde":
          dhtOtilde
        of "Oumlaut":
          dhtOumlaut
        of "times":
          dhtTimes
        of "Oslash":
          dhtOslash
        of "Ugrave":
          dhtUgrave
        of "Uacute":
          dhtUacute
        of "Ucirc":
          dhtUcirc
        of "Uumlaut":
          dhtUumlaut
        of "Yacute":
          dhtYacute
        of "THORN":
          dhtTHORN
        of "szlig":
          dhtSzlig
        of "agrave":
          dhtAgrave1
        of "aacute":
          dhtAacute1
        of "acirc":
          dhtAcirc1
        of "atilde":
          dhtAtilde1
        of "aumlaut":
          dhtAumlaut1
        of "aring":
          dhtAring1
        of "aelig":
          dhtAelig1
        of "ccedil":
          dhtCcedil1
        of "egrave":
          dhtEgrave1
        of "eacute":
          dhtEacute1
        of "ecirc":
          dhtEcirc1
        of "eumlaut":
          dhtEumlaut1
        of "igrave":
          dhtIgrave1
        of "iacute":
          dhtIacute1
        of "icirc":
          dhtIcirc1
        of "iumlaut":
          dhtIumlaut1
        of "eth":
          dhtEth1
        of "ntilde":
          dhtNtilde1
        of "ograve":
          dhtOgrave1
        of "oacute":
          dhtOacute1
        of "ocirc":
          dhtOcirc1
        of "otilde":
          dhtOtilde1
        of "oumlaut":
          dhtOumlaut1
        of "divide":
          dhtDivide
        of "oslash":
          dhtOslash1
        of "ugrave":
          dhtUgrave1
        of "uacute":
          dhtUacute1
        of "ucirc":
          dhtUcirc1
        of "uumlaut":
          dhtUumlaut1
        of "yacute":
          dhtYacute1
        of "thorn":
          dhtThorn1
        of "yumlaut":
          dhtYumlaut
        of "fnof":
          dhtFnof
        of "Alpha":
          dhtAlpha
        of "Beta":
          dhtBeta
        of "Gamma":
          dhtGamma
        of "Delta":
          dhtDelta
        of "Epsilon":
          dhtEpsilon
        of "Zeta":
          dhtZeta
        of "Eta":
          dhtEta
        of "Theta":
          dhtTheta
        of "Iota":
          dhtIota
        of "Kappa":
          dhtKappa
        of "Lambda":
          dhtLambda
        of "Mu":
          dhtMu
        of "Nu":
          dhtNu
        of "Xi":
          dhtXi
        of "Omicron":
          dhtOmicron
        of "Pi":
          dhtPi
        of "Rho":
          dhtRho
        of "Sigma":
          dhtSigma
        of "Tau":
          dhtTau
        of "Upsilon":
          dhtUpsilon
        of "Phi":
          dhtPhi
        of "Chi":
          dhtChi
        of "Psi":
          dhtPsi
        of "Omega":
          dhtOmega
        of "alpha":
          dhtAlpha1
        of "beta":
          dhtBeta1
        of "gamma":
          dhtGamma1
        of "delta":
          dhtDelta1
        of "epsilon":
          dhtEpsilon1
        of "zeta":
          dhtZeta1
        of "eta":
          dhtEta1
        of "theta":
          dhtTheta1
        of "iota":
          dhtIota1
        of "kappa":
          dhtKappa1
        of "lambda":
          dhtLambda1
        of "mu":
          dhtMu1
        of "nu":
          dhtNu1
        of "xi":
          dhtXi1
        of "omicron":
          dhtOmicron1
        of "pi":
          dhtPi1
        of "rho":
          dhtRho1
        of "sigmaf":
          dhtSigmaf
        of "sigma":
          dhtSigma1
        of "tau":
          dhtTau1
        of "upsilon":
          dhtUpsilon1
        of "phi":
          dhtPhi1
        of "chi":
          dhtChi1
        of "psi":
          dhtPsi1
        of "omega":
          dhtOmega1
        of "thetasym":
          dhtThetasym
        of "upsih":
          dhtUpsih
        of "piv":
          dhtPiv
        of "bull":
          dhtBull
        of "hellip":
          dhtHellip
        of "prime":
          dhtPrime
        of "Prime":
          dhtPrime1
        of "oline":
          dhtOline
        of "frasl":
          dhtFrasl
        of "weierp":
          dhtWeierp
        of "imaginary":
          dhtImaginary
        of "real":
          dhtReal
        of "trademark":
          dhtTrademark
        of "alefsym":
          dhtAlefsym
        of "larr":
          dhtLarr
        of "uarr":
          dhtUarr
        of "rarr":
          dhtRarr
        of "darr":
          dhtDarr
        of "harr":
          dhtHarr
        of "crarr":
          dhtCrarr
        of "lArr":
          dhtLArr1
        of "uArr":
          dhtUArr1
        of "rArr":
          dhtRArr1
        of "dArr":
          dhtDArr1
        of "hArr":
          dhtHArr1
        of "forall":
          dhtForall
        of "part":
          dhtPart
        of "exist":
          dhtExist
        of "empty":
          dhtEmpty
        of "nabla":
          dhtNabla
        of "isin":
          dhtIsin
        of "notin":
          dhtNotin
        of "ni":
          dhtNi
        of "prod":
          dhtProd
        of "sum":
          dhtSum
        of "minus":
          dhtMinus
        of "lowast":
          dhtLowast
        of "radic":
          dhtRadic
        of "prop":
          dhtProp
        of "infin":
          dhtInfin
        of "ang":
          dhtAng
        of "and":
          dhtAnd
        of "or":
          dhtOr
        of "cap":
          dhtCap
        of "cup":
          dhtCup
        of "int":
          dhtInt
        of "there4":
          dhtThere4
        of "sim":
          dhtSim
        of "cong":
          dhtCong
        of "asymp":
          dhtAsymp
        of "ne":
          dhtNe
        of "equiv":
          dhtEquiv
        of "le":
          dhtLe
        of "ge":
          dhtGe
        of "sub":
          dhtSub
        of "sup":
          dhtSup
        of "nsub":
          dhtNsub
        of "sube":
          dhtSube
        of "supe":
          dhtSupe
        of "oplus":
          dhtOplus
        of "otimes":
          dhtOtimes
        of "perp":
          dhtPerp
        of "sdot":
          dhtSdot
        of "lceil":
          dhtLceil
        of "rceil":
          dhtRceil
        of "lfloor":
          dhtLfloor
        of "rfloor":
          dhtRfloor
        of "lang":
          dhtLang
        of "rang":
          dhtRang
        of "loz":
          dhtLoz
        of "spades":
          dhtSpades
        of "clubs":
          dhtClubs
        of "hearts":
          dhtHearts
        of "diams":
          dhtDiams
        of "OElig":
          dhtOElig
        of "oelig":
          dhtOelig1
        of "Scaron":
          dhtScaron
        of "scaron":
          dhtScaron1
        of "Yumlaut":
          dhtYumlaut1
        of "circ":
          dhtCirc
        of "tilde":
          dhtTilde
        of "ensp":
          dhtEnsp
        of "emsp":
          dhtEmsp
        of "thinsp":
          dhtThinsp
        of "zwnj":
          dhtZwnj
        of "zwj":
          dhtZwj
        of "lrm":
          dhtLrm
        of "rlm":
          dhtRlm
        of "ndash":
          dhtNdash
        of "mdash":
          dhtMdash
        of "lsquo":
          dhtLsquo
        of "rsquo":
          dhtRsquo
        of "sbquo":
          dhtSbquo
        of "ldquo":
          dhtLdquo
        of "rdquo":
          dhtRdquo
        of "bdquo":
          dhtBdquo
        of "dagger":
          dhtDagger
        of "Dagger":
          dhtDagger1
        of "permil":
          dhtPermil
        of "lsaquo":
          dhtLsaquo
        of "rsaquo":
          dhtRsaquo
        of "euro":
          dhtEuro
        of "tm":
          dhtTm
        else:
          dhtLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocHeadingTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocImageType): int =
  item.xsdChoice.len


iterator items*(item: DocImageType): DocImageTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocImageType; idx: int): DocImageTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocImageType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "type":
        loadXml(parser, target.fType, "type")
      of "name":
        loadXml(parser, target.name, "name")
      of "width":
        loadXml(parser, target.width, "width")
      of "height":
        loadXml(parser, target.height, "height")
      of "alt":
        loadXml(parser, target.alt, "alt")
      of "inline":
        loadXml(parser, target.inline, "inline")
      of "caption":
        loadXml(parser, target.caption, "caption")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocImageTypeBody(kind: doitMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocImageTypeBody(kind: doitUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          doitBold
        of "s":
          doitS
        of "strike":
          doitStrike
        of "underline":
          doitUnderline
        of "emphasis":
          doitEmphasis
        of "computeroutput":
          doitComputeroutput
        of "subscript":
          doitSubscript
        of "superscript":
          doitSuperscript
        of "center":
          doitCenter
        of "small":
          doitSmall
        of "del":
          doitDel
        of "ins":
          doitIns
        else:
          doitBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocImageTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocImageTypeBody(kind: doitHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          doitManonly
        of "xmlonly":
          doitXmlonly
        of "rtfonly":
          doitRtfonly
        of "latexonly":
          doitLatexonly
        of "docbookonly":
          doitDocbookonly
        else:
          doitManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocImageTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          doitImage
        of "dot":
          doitDot
        of "msc":
          doitMsc
        of "plantuml":
          doitPlantuml
        else:
          doitImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocImageTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocImageTypeBody(kind: doitAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocImageTypeBody(kind: doitFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocImageTypeBody(kind: doitRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocImageTypeBody(kind: doitEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          doitLinebreak
        of "nonbreakablespace":
          doitNonbreakablespace
        of "iexcl":
          doitIexcl
        of "cent":
          doitCent
        of "pound":
          doitPound
        of "curren":
          doitCurren
        of "yen":
          doitYen
        of "brvbar":
          doitBrvbar
        of "sect":
          doitSect
        of "umlaut":
          doitUmlaut
        of "copy":
          doitCopy
        of "ordf":
          doitOrdf
        of "laquo":
          doitLaquo
        of "not":
          doitNot
        of "shy":
          doitShy
        of "registered":
          doitRegistered
        of "macr":
          doitMacr
        of "deg":
          doitDeg
        of "plusmn":
          doitPlusmn
        of "sup2":
          doitSup2
        of "sup3":
          doitSup3
        of "acute":
          doitAcute
        of "micro":
          doitMicro
        of "para":
          doitPara
        of "middot":
          doitMiddot
        of "cedil":
          doitCedil
        of "sup1":
          doitSup1
        of "ordm":
          doitOrdm
        of "raquo":
          doitRaquo
        of "frac14":
          doitFrac14
        of "frac12":
          doitFrac12
        of "frac34":
          doitFrac34
        of "iquest":
          doitIquest
        of "Agrave":
          doitAgrave
        of "Aacute":
          doitAacute
        of "Acirc":
          doitAcirc
        of "Atilde":
          doitAtilde
        of "Aumlaut":
          doitAumlaut
        of "Aring":
          doitAring
        of "AElig":
          doitAElig
        of "Ccedil":
          doitCcedil
        of "Egrave":
          doitEgrave
        of "Eacute":
          doitEacute
        of "Ecirc":
          doitEcirc
        of "Eumlaut":
          doitEumlaut
        of "Igrave":
          doitIgrave
        of "Iacute":
          doitIacute
        of "Icirc":
          doitIcirc
        of "Iumlaut":
          doitIumlaut
        of "ETH":
          doitETH
        of "Ntilde":
          doitNtilde
        of "Ograve":
          doitOgrave
        of "Oacute":
          doitOacute
        of "Ocirc":
          doitOcirc
        of "Otilde":
          doitOtilde
        of "Oumlaut":
          doitOumlaut
        of "times":
          doitTimes
        of "Oslash":
          doitOslash
        of "Ugrave":
          doitUgrave
        of "Uacute":
          doitUacute
        of "Ucirc":
          doitUcirc
        of "Uumlaut":
          doitUumlaut
        of "Yacute":
          doitYacute
        of "THORN":
          doitTHORN
        of "szlig":
          doitSzlig
        of "agrave":
          doitAgrave1
        of "aacute":
          doitAacute1
        of "acirc":
          doitAcirc1
        of "atilde":
          doitAtilde1
        of "aumlaut":
          doitAumlaut1
        of "aring":
          doitAring1
        of "aelig":
          doitAelig1
        of "ccedil":
          doitCcedil1
        of "egrave":
          doitEgrave1
        of "eacute":
          doitEacute1
        of "ecirc":
          doitEcirc1
        of "eumlaut":
          doitEumlaut1
        of "igrave":
          doitIgrave1
        of "iacute":
          doitIacute1
        of "icirc":
          doitIcirc1
        of "iumlaut":
          doitIumlaut1
        of "eth":
          doitEth1
        of "ntilde":
          doitNtilde1
        of "ograve":
          doitOgrave1
        of "oacute":
          doitOacute1
        of "ocirc":
          doitOcirc1
        of "otilde":
          doitOtilde1
        of "oumlaut":
          doitOumlaut1
        of "divide":
          doitDivide
        of "oslash":
          doitOslash1
        of "ugrave":
          doitUgrave1
        of "uacute":
          doitUacute1
        of "ucirc":
          doitUcirc1
        of "uumlaut":
          doitUumlaut1
        of "yacute":
          doitYacute1
        of "thorn":
          doitThorn1
        of "yumlaut":
          doitYumlaut
        of "fnof":
          doitFnof
        of "Alpha":
          doitAlpha
        of "Beta":
          doitBeta
        of "Gamma":
          doitGamma
        of "Delta":
          doitDelta
        of "Epsilon":
          doitEpsilon
        of "Zeta":
          doitZeta
        of "Eta":
          doitEta
        of "Theta":
          doitTheta
        of "Iota":
          doitIota
        of "Kappa":
          doitKappa
        of "Lambda":
          doitLambda
        of "Mu":
          doitMu
        of "Nu":
          doitNu
        of "Xi":
          doitXi
        of "Omicron":
          doitOmicron
        of "Pi":
          doitPi
        of "Rho":
          doitRho
        of "Sigma":
          doitSigma
        of "Tau":
          doitTau
        of "Upsilon":
          doitUpsilon
        of "Phi":
          doitPhi
        of "Chi":
          doitChi
        of "Psi":
          doitPsi
        of "Omega":
          doitOmega
        of "alpha":
          doitAlpha1
        of "beta":
          doitBeta1
        of "gamma":
          doitGamma1
        of "delta":
          doitDelta1
        of "epsilon":
          doitEpsilon1
        of "zeta":
          doitZeta1
        of "eta":
          doitEta1
        of "theta":
          doitTheta1
        of "iota":
          doitIota1
        of "kappa":
          doitKappa1
        of "lambda":
          doitLambda1
        of "mu":
          doitMu1
        of "nu":
          doitNu1
        of "xi":
          doitXi1
        of "omicron":
          doitOmicron1
        of "pi":
          doitPi1
        of "rho":
          doitRho1
        of "sigmaf":
          doitSigmaf
        of "sigma":
          doitSigma1
        of "tau":
          doitTau1
        of "upsilon":
          doitUpsilon1
        of "phi":
          doitPhi1
        of "chi":
          doitChi1
        of "psi":
          doitPsi1
        of "omega":
          doitOmega1
        of "thetasym":
          doitThetasym
        of "upsih":
          doitUpsih
        of "piv":
          doitPiv
        of "bull":
          doitBull
        of "hellip":
          doitHellip
        of "prime":
          doitPrime
        of "Prime":
          doitPrime1
        of "oline":
          doitOline
        of "frasl":
          doitFrasl
        of "weierp":
          doitWeierp
        of "imaginary":
          doitImaginary
        of "real":
          doitReal
        of "trademark":
          doitTrademark
        of "alefsym":
          doitAlefsym
        of "larr":
          doitLarr
        of "uarr":
          doitUarr
        of "rarr":
          doitRarr
        of "darr":
          doitDarr
        of "harr":
          doitHarr
        of "crarr":
          doitCrarr
        of "lArr":
          doitLArr1
        of "uArr":
          doitUArr1
        of "rArr":
          doitRArr1
        of "dArr":
          doitDArr1
        of "hArr":
          doitHArr1
        of "forall":
          doitForall
        of "part":
          doitPart
        of "exist":
          doitExist
        of "empty":
          doitEmpty
        of "nabla":
          doitNabla
        of "isin":
          doitIsin
        of "notin":
          doitNotin
        of "ni":
          doitNi
        of "prod":
          doitProd
        of "sum":
          doitSum
        of "minus":
          doitMinus
        of "lowast":
          doitLowast
        of "radic":
          doitRadic
        of "prop":
          doitProp
        of "infin":
          doitInfin
        of "ang":
          doitAng
        of "and":
          doitAnd
        of "or":
          doitOr
        of "cap":
          doitCap
        of "cup":
          doitCup
        of "int":
          doitInt
        of "there4":
          doitThere4
        of "sim":
          doitSim
        of "cong":
          doitCong
        of "asymp":
          doitAsymp
        of "ne":
          doitNe
        of "equiv":
          doitEquiv
        of "le":
          doitLe
        of "ge":
          doitGe
        of "sub":
          doitSub
        of "sup":
          doitSup
        of "nsub":
          doitNsub
        of "sube":
          doitSube
        of "supe":
          doitSupe
        of "oplus":
          doitOplus
        of "otimes":
          doitOtimes
        of "perp":
          doitPerp
        of "sdot":
          doitSdot
        of "lceil":
          doitLceil
        of "rceil":
          doitRceil
        of "lfloor":
          doitLfloor
        of "rfloor":
          doitRfloor
        of "lang":
          doitLang
        of "rang":
          doitRang
        of "loz":
          doitLoz
        of "spades":
          doitSpades
        of "clubs":
          doitClubs
        of "hearts":
          doitHearts
        of "diams":
          doitDiams
        of "OElig":
          doitOElig
        of "oelig":
          doitOelig1
        of "Scaron":
          doitScaron
        of "scaron":
          doitScaron1
        of "Yumlaut":
          doitYumlaut1
        of "circ":
          doitCirc
        of "tilde":
          doitTilde
        of "ensp":
          doitEnsp
        of "emsp":
          doitEmsp
        of "thinsp":
          doitThinsp
        of "zwnj":
          doitZwnj
        of "zwj":
          doitZwj
        of "lrm":
          doitLrm
        of "rlm":
          doitRlm
        of "ndash":
          doitNdash
        of "mdash":
          doitMdash
        of "lsquo":
          doitLsquo
        of "rsquo":
          doitRsquo
        of "sbquo":
          doitSbquo
        of "ldquo":
          doitLdquo
        of "rdquo":
          doitRdquo
        of "bdquo":
          doitBdquo
        of "dagger":
          doitDagger
        of "Dagger":
          doitDagger1
        of "permil":
          doitPermil
        of "lsaquo":
          doitLsaquo
        of "rsaquo":
          doitRsaquo
        of "euro":
          doitEuro
        of "tm":
          doitTm
        else:
          doitLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocImageTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocTocItemType): int =
  item.xsdChoice.len


iterator items*(item: DocTocItemType): DocTocItemTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocTocItemType; idx: int): DocTocItemTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocTocItemType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice,
          DocTocItemTypeBody(kind: dtitMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ulink":
        ## 617:12:xml_to_types.nim
        var tmp: DocURLLink
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTocItemTypeBody(kind: dtitUlink, docURLLink: tmp))
      of "bold", "s", "strike", "underline", "emphasis", "computeroutput",
         "subscript", "superscript", "center", "small", "del", "ins":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "bold":
          dtitBold
        of "s":
          dtitS
        of "strike":
          dtitStrike
        of "underline":
          dtitUnderline
        of "emphasis":
          dtitEmphasis
        of "computeroutput":
          dtitComputeroutput
        of "subscript":
          dtitSubscript
        of "superscript":
          dtitSuperscript
        of "center":
          dtitCenter
        of "small":
          dtitSmall
        of "del":
          dtitDel
        of "ins":
          dtitIns
        else:
          dtitBold
        var tmp: DocMarkupType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTocItemTypeBody(kind: kind)
        tmp2.docMarkupType = tmp
        add(target.xsdChoice, tmp2)
      of "htmlonly":
        ## 617:12:xml_to_types.nim
        var tmp: DocHtmlOnlyType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTocItemTypeBody(kind: dtitHtmlonly, docHtmlOnlyType: tmp))
      of "manonly", "xmlonly", "rtfonly", "latexonly", "docbookonly":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "manonly":
          dtitManonly
        of "xmlonly":
          dtitXmlonly
        of "rtfonly":
          dtitRtfonly
        of "latexonly":
          dtitLatexonly
        of "docbookonly":
          dtitDocbookonly
        else:
          dtitManonly
        var tmp: string
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTocItemTypeBody(kind: kind)
        tmp2.fString = tmp
        add(target.xsdChoice, tmp2)
      of "image", "dot", "msc", "plantuml":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "image":
          dtitImage
        of "dot":
          dtitDot
        of "msc":
          dtitMsc
        of "plantuml":
          dtitPlantuml
        else:
          dtitImage
        var tmp: DocImageType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTocItemTypeBody(kind: kind)
        tmp2.docImageType = tmp
        add(target.xsdChoice, tmp2)
      of "anchor":
        ## 617:12:xml_to_types.nim
        var tmp: DocAnchorType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTocItemTypeBody(kind: dtitAnchor, docAnchorType: tmp))
      of "formula":
        ## 617:12:xml_to_types.nim
        var tmp: DocFormulaType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTocItemTypeBody(kind: dtitFormula, docFormulaType: tmp))
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: DocRefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTocItemTypeBody(kind: dtitRef, docRefTextType: tmp))
      of "emoji":
        ## 617:12:xml_to_types.nim
        var tmp: DocEmojiType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice,
            DocTocItemTypeBody(kind: dtitEmoji, docEmojiType: tmp))
      of "linebreak", "nonbreakablespace", "iexcl", "cent", "pound", "curren",
         "yen", "brvbar", "sect", "umlaut", "copy", "ordf", "laquo", "not",
         "shy", "registered", "macr", "deg", "plusmn", "sup2", "sup3", "acute",
         "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14",
         "frac12", "frac34", "iquest", "Agrave", "Aacute", "Acirc", "Atilde",
         "Aumlaut", "Aring", "AElig", "Ccedil", "Egrave", "Eacute", "Ecirc",
         "Eumlaut", "Igrave", "Iacute", "Icirc", "Iumlaut", "ETH", "Ntilde",
         "Ograve", "Oacute", "Ocirc", "Otilde", "Oumlaut", "times", "Oslash",
         "Ugrave", "Uacute", "Ucirc", "Uumlaut", "Yacute", "THORN", "szlig",
         "agrave", "aacute", "acirc", "atilde", "aumlaut", "aring", "aelig",
         "ccedil", "egrave", "eacute", "ecirc", "eumlaut", "igrave", "iacute",
         "icirc", "iumlaut", "eth", "ntilde", "ograve", "oacute", "ocirc",
         "otilde", "oumlaut", "divide", "oslash", "ugrave", "uacute", "ucirc",
         "uumlaut", "yacute", "thorn", "yumlaut", "fnof", "Alpha", "Beta",
         "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa",
         "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau",
         "Upsilon", "Phi", "Chi", "Psi", "Omega", "alpha", "beta", "gamma",
         "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda",
         "mu", "nu", "xi", "omicron", "pi", "rho", "sigmaf", "sigma", "tau",
         "upsilon", "phi", "chi", "psi", "omega", "thetasym", "upsih", "piv",
         "bull", "hellip", "prime", "Prime", "oline", "frasl", "weierp",
         "imaginary", "real", "trademark", "alefsym", "larr", "uarr", "rarr",
         "darr", "harr", "crarr", "lArr", "uArr", "rArr", "dArr", "hArr",
         "forall", "part", "exist", "empty", "nabla", "isin", "notin", "ni",
         "prod", "sum", "minus", "lowast", "radic", "prop", "infin", "ang",
         "and", "or", "cap", "cup", "int", "there4", "sim", "cong", "asymp",
         "ne", "equiv", "le", "ge", "sub", "sup", "nsub", "sube", "supe",
         "oplus", "otimes", "perp", "sdot", "lceil", "rceil", "lfloor",
         "rfloor", "lang", "rang", "loz", "spades", "clubs", "hearts", "diams",
         "OElig", "oelig", "Scaron", "scaron", "Yumlaut", "circ", "tilde",
         "ensp", "emsp", "thinsp", "zwnj", "zwj", "lrm", "rlm", "ndash",
         "mdash", "lsquo", "rsquo", "sbquo", "ldquo", "rdquo", "bdquo",
         "dagger", "Dagger", "permil", "lsaquo", "rsaquo", "euro", "tm":
        ## 628:12:xml_to_types.nim
        let kind = case parser.elementName()
        of "linebreak":
          dtitLinebreak
        of "nonbreakablespace":
          dtitNonbreakablespace
        of "iexcl":
          dtitIexcl
        of "cent":
          dtitCent
        of "pound":
          dtitPound
        of "curren":
          dtitCurren
        of "yen":
          dtitYen
        of "brvbar":
          dtitBrvbar
        of "sect":
          dtitSect
        of "umlaut":
          dtitUmlaut
        of "copy":
          dtitCopy
        of "ordf":
          dtitOrdf
        of "laquo":
          dtitLaquo
        of "not":
          dtitNot
        of "shy":
          dtitShy
        of "registered":
          dtitRegistered
        of "macr":
          dtitMacr
        of "deg":
          dtitDeg
        of "plusmn":
          dtitPlusmn
        of "sup2":
          dtitSup2
        of "sup3":
          dtitSup3
        of "acute":
          dtitAcute
        of "micro":
          dtitMicro
        of "para":
          dtitPara
        of "middot":
          dtitMiddot
        of "cedil":
          dtitCedil
        of "sup1":
          dtitSup1
        of "ordm":
          dtitOrdm
        of "raquo":
          dtitRaquo
        of "frac14":
          dtitFrac14
        of "frac12":
          dtitFrac12
        of "frac34":
          dtitFrac34
        of "iquest":
          dtitIquest
        of "Agrave":
          dtitAgrave
        of "Aacute":
          dtitAacute
        of "Acirc":
          dtitAcirc
        of "Atilde":
          dtitAtilde
        of "Aumlaut":
          dtitAumlaut
        of "Aring":
          dtitAring
        of "AElig":
          dtitAElig
        of "Ccedil":
          dtitCcedil
        of "Egrave":
          dtitEgrave
        of "Eacute":
          dtitEacute
        of "Ecirc":
          dtitEcirc
        of "Eumlaut":
          dtitEumlaut
        of "Igrave":
          dtitIgrave
        of "Iacute":
          dtitIacute
        of "Icirc":
          dtitIcirc
        of "Iumlaut":
          dtitIumlaut
        of "ETH":
          dtitETH
        of "Ntilde":
          dtitNtilde
        of "Ograve":
          dtitOgrave
        of "Oacute":
          dtitOacute
        of "Ocirc":
          dtitOcirc
        of "Otilde":
          dtitOtilde
        of "Oumlaut":
          dtitOumlaut
        of "times":
          dtitTimes
        of "Oslash":
          dtitOslash
        of "Ugrave":
          dtitUgrave
        of "Uacute":
          dtitUacute
        of "Ucirc":
          dtitUcirc
        of "Uumlaut":
          dtitUumlaut
        of "Yacute":
          dtitYacute
        of "THORN":
          dtitTHORN
        of "szlig":
          dtitSzlig
        of "agrave":
          dtitAgrave1
        of "aacute":
          dtitAacute1
        of "acirc":
          dtitAcirc1
        of "atilde":
          dtitAtilde1
        of "aumlaut":
          dtitAumlaut1
        of "aring":
          dtitAring1
        of "aelig":
          dtitAelig1
        of "ccedil":
          dtitCcedil1
        of "egrave":
          dtitEgrave1
        of "eacute":
          dtitEacute1
        of "ecirc":
          dtitEcirc1
        of "eumlaut":
          dtitEumlaut1
        of "igrave":
          dtitIgrave1
        of "iacute":
          dtitIacute1
        of "icirc":
          dtitIcirc1
        of "iumlaut":
          dtitIumlaut1
        of "eth":
          dtitEth1
        of "ntilde":
          dtitNtilde1
        of "ograve":
          dtitOgrave1
        of "oacute":
          dtitOacute1
        of "ocirc":
          dtitOcirc1
        of "otilde":
          dtitOtilde1
        of "oumlaut":
          dtitOumlaut1
        of "divide":
          dtitDivide
        of "oslash":
          dtitOslash1
        of "ugrave":
          dtitUgrave1
        of "uacute":
          dtitUacute1
        of "ucirc":
          dtitUcirc1
        of "uumlaut":
          dtitUumlaut1
        of "yacute":
          dtitYacute1
        of "thorn":
          dtitThorn1
        of "yumlaut":
          dtitYumlaut
        of "fnof":
          dtitFnof
        of "Alpha":
          dtitAlpha
        of "Beta":
          dtitBeta
        of "Gamma":
          dtitGamma
        of "Delta":
          dtitDelta
        of "Epsilon":
          dtitEpsilon
        of "Zeta":
          dtitZeta
        of "Eta":
          dtitEta
        of "Theta":
          dtitTheta
        of "Iota":
          dtitIota
        of "Kappa":
          dtitKappa
        of "Lambda":
          dtitLambda
        of "Mu":
          dtitMu
        of "Nu":
          dtitNu
        of "Xi":
          dtitXi
        of "Omicron":
          dtitOmicron
        of "Pi":
          dtitPi
        of "Rho":
          dtitRho
        of "Sigma":
          dtitSigma
        of "Tau":
          dtitTau
        of "Upsilon":
          dtitUpsilon
        of "Phi":
          dtitPhi
        of "Chi":
          dtitChi
        of "Psi":
          dtitPsi
        of "Omega":
          dtitOmega
        of "alpha":
          dtitAlpha1
        of "beta":
          dtitBeta1
        of "gamma":
          dtitGamma1
        of "delta":
          dtitDelta1
        of "epsilon":
          dtitEpsilon1
        of "zeta":
          dtitZeta1
        of "eta":
          dtitEta1
        of "theta":
          dtitTheta1
        of "iota":
          dtitIota1
        of "kappa":
          dtitKappa1
        of "lambda":
          dtitLambda1
        of "mu":
          dtitMu1
        of "nu":
          dtitNu1
        of "xi":
          dtitXi1
        of "omicron":
          dtitOmicron1
        of "pi":
          dtitPi1
        of "rho":
          dtitRho1
        of "sigmaf":
          dtitSigmaf
        of "sigma":
          dtitSigma1
        of "tau":
          dtitTau1
        of "upsilon":
          dtitUpsilon1
        of "phi":
          dtitPhi1
        of "chi":
          dtitChi1
        of "psi":
          dtitPsi1
        of "omega":
          dtitOmega1
        of "thetasym":
          dtitThetasym
        of "upsih":
          dtitUpsih
        of "piv":
          dtitPiv
        of "bull":
          dtitBull
        of "hellip":
          dtitHellip
        of "prime":
          dtitPrime
        of "Prime":
          dtitPrime1
        of "oline":
          dtitOline
        of "frasl":
          dtitFrasl
        of "weierp":
          dtitWeierp
        of "imaginary":
          dtitImaginary
        of "real":
          dtitReal
        of "trademark":
          dtitTrademark
        of "alefsym":
          dtitAlefsym
        of "larr":
          dtitLarr
        of "uarr":
          dtitUarr
        of "rarr":
          dtitRarr
        of "darr":
          dtitDarr
        of "harr":
          dtitHarr
        of "crarr":
          dtitCrarr
        of "lArr":
          dtitLArr1
        of "uArr":
          dtitUArr1
        of "rArr":
          dtitRArr1
        of "dArr":
          dtitDArr1
        of "hArr":
          dtitHArr1
        of "forall":
          dtitForall
        of "part":
          dtitPart
        of "exist":
          dtitExist
        of "empty":
          dtitEmpty
        of "nabla":
          dtitNabla
        of "isin":
          dtitIsin
        of "notin":
          dtitNotin
        of "ni":
          dtitNi
        of "prod":
          dtitProd
        of "sum":
          dtitSum
        of "minus":
          dtitMinus
        of "lowast":
          dtitLowast
        of "radic":
          dtitRadic
        of "prop":
          dtitProp
        of "infin":
          dtitInfin
        of "ang":
          dtitAng
        of "and":
          dtitAnd
        of "or":
          dtitOr
        of "cap":
          dtitCap
        of "cup":
          dtitCup
        of "int":
          dtitInt
        of "there4":
          dtitThere4
        of "sim":
          dtitSim
        of "cong":
          dtitCong
        of "asymp":
          dtitAsymp
        of "ne":
          dtitNe
        of "equiv":
          dtitEquiv
        of "le":
          dtitLe
        of "ge":
          dtitGe
        of "sub":
          dtitSub
        of "sup":
          dtitSup
        of "nsub":
          dtitNsub
        of "sube":
          dtitSube
        of "supe":
          dtitSupe
        of "oplus":
          dtitOplus
        of "otimes":
          dtitOtimes
        of "perp":
          dtitPerp
        of "sdot":
          dtitSdot
        of "lceil":
          dtitLceil
        of "rceil":
          dtitRceil
        of "lfloor":
          dtitLfloor
        of "rfloor":
          dtitRfloor
        of "lang":
          dtitLang
        of "rang":
          dtitRang
        of "loz":
          dtitLoz
        of "spades":
          dtitSpades
        of "clubs":
          dtitClubs
        of "hearts":
          dtitHearts
        of "diams":
          dtitDiams
        of "OElig":
          dtitOElig
        of "oelig":
          dtitOelig1
        of "Scaron":
          dtitScaron
        of "scaron":
          dtitScaron1
        of "Yumlaut":
          dtitYumlaut1
        of "circ":
          dtitCirc
        of "tilde":
          dtitTilde
        of "ensp":
          dtitEnsp
        of "emsp":
          dtitEmsp
        of "thinsp":
          dtitThinsp
        of "zwnj":
          dtitZwnj
        of "zwj":
          dtitZwj
        of "lrm":
          dtitLrm
        of "rlm":
          dtitRlm
        of "ndash":
          dtitNdash
        of "mdash":
          dtitMdash
        of "lsquo":
          dtitLsquo
        of "rsquo":
          dtitRsquo
        of "sbquo":
          dtitSbquo
        of "ldquo":
          dtitLdquo
        of "rdquo":
          dtitRdquo
        of "bdquo":
          dtitBdquo
        of "dagger":
          dtitDagger
        of "Dagger":
          dtitDagger1
        of "permil":
          dtitPermil
        of "lsaquo":
          dtitLsaquo
        of "rsaquo":
          dtitRsaquo
        of "euro":
          dtitEuro
        of "tm":
          dtitTm
        else:
          dtitLinebreak
        var tmp: DocEmptyType
        loadXml(parser, tmp, parser.elementName())
        var tmp2 = DocTocItemTypeBody(kind: kind)
        tmp2.docEmptyType = tmp
        add(target.xsdChoice, tmp2)
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var DocTocListType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "tocitem":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.tocitem, "tocitem")
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


proc loadXml*(parser: var HXmlParser; target: var DocLanguageType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "langid":
        loadXml(parser, target.langid, "langid")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.para, "para")
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


proc loadXml*(parser: var HXmlParser; target: var DocParamListType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
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
      of "parameteritem":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.parameteritem, "parameteritem")
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


proc loadXml*(parser: var HXmlParser; target: var DocParamListItem; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "parameternamelist":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.parameternamelist, "parameternamelist")
      of "parameterdescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.parameterdescription, "parameterdescription")
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


proc loadXml*(parser: var HXmlParser; target: var DocParamNameList; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "parametertype":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.parametertype, "parametertype")
      of "parametername":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.parametername, "parametername")
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


proc len*(item: DocParamType): int =
  item.xsdChoice.len


iterator items*(item: DocParamType): DocParamTypeBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocParamType; idx: int): DocParamTypeBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocParamType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocParamTypeBody(kind: doptMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: RefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocParamTypeBody(kind: doptRef, refTextType: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc len*(item: DocParamName): int =
  item.xsdChoice.len


iterator items*(item: DocParamName): DocParamNameBody =
  for it in item.xsdChoice:
    yield it


proc `[]`*(item: DocParamName; idx: int): DocParamNameBody =
  item.xsdChoice[idx]


proc loadXml*(parser: var HXmlParser; target: var DocParamName; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "direction":
        loadXml(parser, target.direction, "direction")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of XmlEventKind.xmlCharData:
      ## 649:10:xml_to_types.nim
      var tmp: string
      parseXsdString(tmp, parser, "")
      add(target.xsdChoice, DocParamNameBody(kind: dpnMixedStr, mixedStr: tmp))
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "ref":
        ## 617:12:xml_to_types.nim
        var tmp: RefTextType
        loadXml(parser, tmp, parser.elementName())
        add(target.xsdChoice, DocParamNameBody(kind: dpnRef, refTextType: tmp))
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
    of {XmlEventKind.xmlError, XmlEventKind.xmlEof, XmlEventKind.xmlWhitespace,
        XmlEventKind.xmlComment, XmlEventKind.xmlPI, XmlEventKind.xmlCData,
        XmlEventKind.xmlEntity, XmlEventKind.xmlSpecial}:
      ## 736:6:xml_to_types.nim
      echo parser.displayAt()
      assert false


proc loadXml*(parser: var HXmlParser; target: var DocXRefSectType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "id":
        loadXml(parser, target.id, "id")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "xreftitle":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.xreftitle, "xreftitle")
      of "xrefdescription":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.xrefdescription, "xrefdescription")
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


proc loadXml*(parser: var HXmlParser; target: var DocCopyType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "link":
        loadXml(parser, target.link, "link")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.para, "para")
      of "sect1":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.sect1, "sect1")
      of "internal":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.internal, "internal")
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


proc loadXml*(parser: var HXmlParser; target: var DocBlockQuoteType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.para, "para")
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


proc loadXml*(parser: var HXmlParser; target: var DocParBlockType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "para":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.para, "para")
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


proc loadXml*(parser: var HXmlParser; target: var DocEmptyType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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


proc loadXml*(parser: var HXmlParser; target: var TableofcontentsType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
      of "tocsect":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.tocsect, "tocsect")
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


proc loadXml*(parser: var HXmlParser; target: var TableofcontentsKindType;
              tag: string; inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
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
      of "reference":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.reference, "reference")
      of "tableofcontents":
        ## 707:48:xml_to_types.nim 
        loadXml(parser, target.tableofcontents, "tableofcontents")
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


proc loadXml*(parser: var HXmlParser; target: var DocEmojiType; tag: string;
              inMixed: bool = false) =
  ## 743:4:xml_to_types.nim
  next(parser)
  while true:
    case parser.kind
    of XmlEventKind.xmlAttribute:
      case parser.attrKey
      of "name":
        loadXml(parser, target.name, "name")
      of "unicode":
        loadXml(parser, target.unicode, "unicode")
      else:
        ## 553:4:xml_to_types.nim
        if not(startsWith(parser.attrKey(), ["xmlns:", "xsi:", "xml:"])):
          raiseUnexpectedAttribute(parser)
        else:
          parser.next()
    of {XmlEventKind.xmlElementStart, XmlEventKind.xmlElementOpen}:
      case parser.elementName()
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


proc loadXml*(parser: var HXmlParser; target: var DoxBool; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "yes":
    target = dbYes
  of "no":
    target = dbNo
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxGraphRelation; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "include":
    target = dgrInclude
  of "usage":
    target = dgrUsage
  of "template-instance":
    target = dgrTemplateInstance
  of "public-inheritance":
    target = dgrPublicInheritance
  of "protected-inheritance":
    target = dgrProtectedInheritance
  of "private-inheritance":
    target = dgrPrivateInheritance
  of "type-constraint":
    target = dgrTypeConstraint
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxRefKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "compound":
    target = drkCompound
  of "member":
    target = drkMember
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxMemberKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "define":
    target = dmkDefine
  of "property":
    target = dmkProperty
  of "event":
    target = dmkEvent
  of "variable":
    target = dmkVariable
  of "typedef":
    target = dmkTypedef
  of "enum":
    target = dmkEnum
  of "function":
    target = dmkFunction
  of "signal":
    target = dmkSignal
  of "prototype":
    target = dmkPrototype
  of "friend":
    target = dmkFriend
  of "dcop":
    target = dmkDcop
  of "slot":
    target = dmkSlot
  of "interface":
    target = dmkInterface
  of "service":
    target = dmkService
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxProtectionKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "public":
    target = dpkPublic
  of "protected":
    target = dpkProtected
  of "private":
    target = dpkPrivate
  of "package":
    target = dpkPackage
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxRefQualifierKind;
              tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "lvalue":
    target = drqkLvalue
  of "rvalue":
    target = drqkRvalue
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxLanguage; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "Unknown":
    target = dlUnknown
  of "IDL":
    target = dlIDL
  of "Java":
    target = dlJava
  of "C#":
    target = dlCHash
  of "D":
    target = dlD
  of "PHP":
    target = dlPHP
  of "Objective-C":
    target = dlObjectiveC
  of "C++":
    target = dlCPlusPlus
  of "JavaScript":
    target = dlJavaScript
  of "Python":
    target = dlPython
  of "Fortran":
    target = dlFortran
  of "VHDL":
    target = dlVHDL
  of "XML":
    target = dlXML
  of "SQL":
    target = dlSQL
  of "Markdown":
    target = dlMarkdown
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxVirtualKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "non-virtual":
    target = dvkNonVirtual
  of "virtual":
    target = dvkVirtual
  of "pure-virtual":
    target = dvkPureVirtual
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxCompoundKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "class":
    target = dckClass
  of "struct":
    target = dckStruct
  of "union":
    target = dckUnion
  of "interface":
    target = dckInterface
  of "protocol":
    target = dckProtocol
  of "category":
    target = dckCategory
  of "exception":
    target = dckException
  of "service":
    target = dckService
  of "singleton":
    target = dckSingleton
  of "module":
    target = dckModule
  of "type":
    target = dckType
  of "file":
    target = dckFile
  of "namespace":
    target = dckNamespace
  of "group":
    target = dckGroup
  of "page":
    target = dckPage
  of "example":
    target = dckExample
  of "dir":
    target = dckDir
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxSectionKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "user-defined":
    target = dskUserDefined
  of "public-type":
    target = dskPublicType
  of "public-func":
    target = dskPublicFunc
  of "public-attrib":
    target = dskPublicAttrib
  of "public-slot":
    target = dskPublicSlot
  of "signal":
    target = dskSignal
  of "dcop-func":
    target = dskDcopFunc
  of "property":
    target = dskProperty
  of "event":
    target = dskEvent
  of "public-static-func":
    target = dskPublicStaticFunc
  of "public-static-attrib":
    target = dskPublicStaticAttrib
  of "protected-type":
    target = dskProtectedType
  of "protected-func":
    target = dskProtectedFunc
  of "protected-attrib":
    target = dskProtectedAttrib
  of "protected-slot":
    target = dskProtectedSlot
  of "protected-static-func":
    target = dskProtectedStaticFunc
  of "protected-static-attrib":
    target = dskProtectedStaticAttrib
  of "package-type":
    target = dskPackageType
  of "package-func":
    target = dskPackageFunc
  of "package-attrib":
    target = dskPackageAttrib
  of "package-static-func":
    target = dskPackageStaticFunc
  of "package-static-attrib":
    target = dskPackageStaticAttrib
  of "private-type":
    target = dskPrivateType
  of "private-func":
    target = dskPrivateFunc
  of "private-attrib":
    target = dskPrivateAttrib
  of "private-slot":
    target = dskPrivateSlot
  of "private-static-func":
    target = dskPrivateStaticFunc
  of "private-static-attrib":
    target = dskPrivateStaticAttrib
  of "friend":
    target = dskFriend
  of "related":
    target = dskRelated
  of "define":
    target = dskDefine
  of "prototype":
    target = dskPrototype
  of "typedef":
    target = dskTypedef
  of "enum":
    target = dskEnum
  of "func":
    target = dskFunc
  of "var":
    target = dskVar
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxHighlightClass; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "comment":
    target = dhcComment
  of "normal":
    target = dhcNormal
  of "preprocessor":
    target = dhcPreprocessor
  of "keyword":
    target = dhcKeyword
  of "keywordtype":
    target = dhcKeywordtype
  of "keywordflow":
    target = dhcKeywordflow
  of "stringliteral":
    target = dhcStringliteral
  of "charliteral":
    target = dhcCharliteral
  of "vhdlkeyword":
    target = dhcVhdlkeyword
  of "vhdllogic":
    target = dhcVhdllogic
  of "vhdlchar":
    target = dhcVhdlchar
  of "vhdldigit":
    target = dhcVhdldigit
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxSimpleSectKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "see":
    target = dsskSee
  of "return":
    target = dsskReturn
  of "author":
    target = dsskAuthor
  of "authors":
    target = dsskAuthors
  of "version":
    target = dsskVersion
  of "since":
    target = dsskSince
  of "date":
    target = dsskDate
  of "note":
    target = dsskNote
  of "warning":
    target = dsskWarning
  of "pre":
    target = dsskPre
  of "post":
    target = dsskPost
  of "copyright":
    target = dsskCopyright
  of "invariant":
    target = dsskInvariant
  of "remark":
    target = dsskRemark
  of "attention":
    target = dsskAttention
  of "par":
    target = dsskPar
  of "rcs":
    target = dsskRcs
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxVersionNumber; tag: string) =
  ## 800:4:xml_to_types.nim
  var tmp: string
  parseXsdString(tmp, parser)
  target = DoxVersionNumber(tmp)
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxImageKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "html":
    target = dikHtml
  of "latex":
    target = dikLatex
  of "docbook":
    target = dikDocbook
  of "rtf":
    target = dikRtf
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxParamListKind; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "param":
    target = dplkParam
  of "retval":
    target = dplkRetval
  of "exception":
    target = dplkException
  of "templateparam":
    target = dplkTemplateparam
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxCharRange; tag: string) =
  ## 800:4:xml_to_types.nim
  var tmp: string
  parseXsdString(tmp, parser)
  target = DoxCharRange(tmp)
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxParamDir; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "in":
    target = dpdIn
  of "out":
    target = dpdOut
  of "inout":
    target = dpdInout
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxAccessor; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "retain":
    target = daRetain
  of "copy":
    target = daCopy
  of "assign":
    target = daAssign
  of "weak":
    target = daWeak
  of "strong":
    target = daStrong
  of "unretained":
    target = daUnretained
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxAlign; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "left":
    target = daLeft
  of "right":
    target = daRight
  of "center":
    target = daCenter
  parser.next()


proc loadXml*(parser: var HXmlParser; target: var DoxVerticalAlign; tag: string) =
  ## 776:4:xml_to_types.nim
  mixin loadXml
  case parser.strVal
  of "bottom":
    target = dvaBottom
  of "top":
    target = dvaTop
  of "middle":
    target = dvaMiddle
  parser.next()
