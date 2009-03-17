" Vim syntax file
" Language:	AEL (Asterisk Extension Language)
" Maintainer:	C. Chad Wallace <cmdrwalrus@gmail.com>
" Last Change:	2007 June 1
" Version:	0.2

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" Top-level blocks

" globals block
syn match	aelGlobalsKeyword	"\<globals\>" nextgroup=aelGlobalsBlock skipwhite
syn region	aelGlobalsBlock		start="{" end="};" contained contains=aelGlobalVar,aelComment
syn match	aelGlobalVar		"\<\h[0-9A-Za-z_-]*\ze=" contained nextgroup=aelGlobalVarExpression
syn match	aelGlobalVarExpression	".*;" contained contains=aelInterpol,aelExpr,aelString


" macro blocks
syn match	aelMacroKeyword		"\<macro\>" nextgroup=aelMacroName skipwhite
syn match	aelMacroName		"\<\h[0-9A-Za-z_-]*" contained nextgroup=aelMacroParms skipwhite
syn match	aelMacroParms		"(\(\s*\h\w*\(\s*,\s*\h\w*\)*\)\=\s*)" contained contains=aelMacroParm nextgroup=aelMacroBlock skipwhite
syn match	aelMacroParm		"\<\h\w*\>" contained
syn region	aelMacroBlock		start="{" end="};" contained contains=@aelStatement,aelCatch,aelLabel,aelComment

" Things you'll find inside a macro
syn match	aelCatch		"\<catch\>" nextgroup=aelCatchName skipwhite
syn match	aelCatchName		"\<\w\+\>" contained nextgroup=aelCatchBlock skipwhite
syn region	aelCatchBlock		start="{" end="};" contained contains=@aelStatement,aelLabel,aelComment


" context blocks
syn match	aelContextKeyword	"\<context\>" nextgroup=aelContextName skipwhite
syn match	aelContextName		"\<\h[0-9A-Za-z_-]*\>" contained nextgroup=aelContextBlock skipwhite
syn region	aelContextBlock		start="{" end="};" contained contains=aelIncludes,aelIgnorepat,aelExtension,aelComment

" Things you'll find inside a context
syn match	aelIncludes		"\<includes\>" contained nextgroup=aelIncludesBlock skipwhite
syn region	aelIncludesBlock	start="{" end="};" contained contains=aelIncludeContext,aelComment
syn match	aelIncludeContext	"\<\h[0-9A-Za-z_-]*\ze;" contained

syn match	aelIgnorepat		"\<ignorepat\(\s*=>\s*\)\@=" contained nextgroup=aelIgnorepatNeck skipwhite
syn match	aelIgnorepatNeck	"=>" contained nextgroup=aelIgnorepatPattern skipwhite
syn match	aelIgnorepatPattern	"\<[0-9A-Za-z*#\][]*[.!]\=" contained

syn match	aelExtension		"\(ignorepat\)\@!\(_[0-9A-Za-z*#\][]*[.!]\=\|[0-9A-Za-z*]\+\)\(\s*=>\s*\)\@=" contained nextgroup=aelExtensionNeck skipwhite
syn match	aelExtensionNeck	"=>" contained nextgroup=aelExtensionBlock,aelExtensionSingle skipwhite
syn region	aelExtensionBlock	start="{" end="};" contained contains=@aelStatement,aelLabel,aelComment
syn match	aelExtensionSingle	".*;" contained contains=@aelStatement,aelComment


" Dialplan Statements (priorities)
syn cluster	aelStatement		contains=aelSet,aelApplication,aelIf,aelElse,aelSwitch,aelGoto,aelMacroCall

syn match	aelSet			"\<Set\ze\s*(" contained nextgroup=aelSetOpenParen skipwhite
syn match	aelSetOpenParen		"(" contained nextgroup=aelSetVariable skipwhite
syn match	aelSetVariable		"\h\w*\((\h\w*)\)\=\ze=" contained nextgroup=aelSetExpression skipwhite
syn match	aelSetExpression	".*\ze);" contained contains=aelInterpol,aelExpr,aelString

syn match	aelApplication		"\<\(Set\)\@!\h\w*\ze\s*(" contained nextgroup=aelAppOpenParen skipwhite
syn match	aelAppOpenParen		"(" contained nextgroup=aelAppExpression skipwhite
syn match	aelAppExpression	".*\ze);" contained contains=aelInterpol,aelExpr,aelString

syn match	aelIf			"\<if\>" contained nextgroup=aelIfCond skipwhite
syn match	aelIfCond		"(.*)" contained contains=aelInterpol,aelExpr,aelString,aelOperator nextgroup=aelIfBlock,aelIfElseBlock skipwhite
syn region	aelIfBlock		start="{" end="};" contained contains=@aelStatement,aelLabel,aelComment
syn region	aelIfElseBlock		start="{" end="}" contained contains=@aelStatement,aelLabel,aelComment nextgroup=aelElse skipwhite skipnl
syn match	aelElse			"\<else\>" contained nextgroup=aelElseBlock skipwhite
syn region	aelElseBlock		start="{" end="};" contained contains=@aelStatement,aelLabel,aelComment

syn match	aelSwitch		"\<switch\>" contained nextgroup=aelSwitchCond skipwhite
syn match	aelSwitchCond		"(.*)" contained contains=aelInterpol,aelExpr,aelString nextgroup=aelSwitchBlock skipwhite
syn region	aelSwitchBlock		start="{" end="};" contained contains=@aelStatement,aelSwitchBreak,aelSwitchCase,aelSwitchDefault,aelComment
syn match	aelSwitchCase		"\<case\>" contained nextgroup=aelSwitchCaseVal skipwhite
syn match	aelSwitchCaseVal	"\s*\zs.*\ze:" contained contains=aelInterpol,aelExpr
syn match	aelSwitchDefault	"\<default\ze:" contained
syn keyword	aelSwitchBreak		break contained

syn match	aelGoto			"\<goto\>" contained nextgroup=aelGotoTarget skipwhite
syn match	aelGotoTarget		"\(\([0-9A-Za-z_-]\+|\)\=[0-9A-Za-z_-]\+|\)\=[0-9A-Za-z_-]\+" contained contains=aelGotoPipe
syn match	aelGotoPipe		"|" contained

syn match	aelMacroCall		"&\h[0-9A-Za-z_-]*\>" contained nextgroup=aelMacroCallParms
syn match	aelMacroCallParms	"(\(.*\(,.*\)*\)\=)" contained contains=aelMacroCallParm
syn match	aelMacroCallParm	".*" contained contains=aelInterpol,aelString,aelExpr


" Generic
syn match	aelLabel		"\<\h[0-9A-Za-z_-]*\ze:" contained
syn match	aelComment		"//.*"
syn region	aelString		start=/"/ skip=/\\"/ end=/"/ contained contains=aelInterpol,aelExpr
syn region	aelInterpol		matchgroup=aelOperator start="${" end="}" contained contains=aelInterpol,aelExpr
syn region	aelExpr			matchgroup=aelOperator start="$\[" end="\]" contained contains=aelInterpol,aelString,aelOperator
syn match	aelOperator		"[<>=%!^&|+*/-]" contained


" Synchronization
syn sync 	minlines=150
syn sync match 	aelMacroSync		grouphere aelMacroBlock "\<macro\>\s*\<\h[0-9A-Za-z_-]*\>\s*(\(\s*\h\w*\(\s*,\s*\h\w*\)*\)\=\s*)\s*{"
syn sync match 	aelContextSync		grouphere aelContextBlock "\<context\>\s*\<\h[0-9A-Za-z_-]*\>\s*{"
syn sync match 	aelGlobalsSync		grouphere aelGlobalsBlock "\<globals\>\s*{"


" Highlighting

" Keywords
hi def link	aelGoto			aelStatement
hi def link	aelStatement		Statement
hi def link 	aelIf			aelConditional
hi def link 	aelElse			aelConditional
hi def link 	aelSwitch		aelConditional
hi def link	aelConditional		Conditional
hi def link	aelSwitchCase		aelLabel
hi def link	aelSwitchDefault	aelLabel
hi def link	aelCase			aelLabel
hi def link	aelLabel		Label
hi def link	aelCatch		aelException
hi def link	aelException		Exception

hi def link 	aelIncludes		aelKeyword
hi def link 	aelIgnorepat		aelKeyword
hi def link 	aelGlobalsKeyword	aelKeyword
hi def link 	aelMacroKeyword		aelKeyword
hi def link 	aelContextKeyword	aelKeyword
hi def link	aelSwitchBreak		aelKeyword
hi def link 	aelKeyword		Keyword

hi def link	aelIgnorepatNeck	Operator
hi def link	aelExtensionNeck	Operator
hi def link	aelGotoPipe		Operator
hi def link	aelOperator		Operator

" Literals
hi def link	aelSwitchCaseVal	String
hi def link 	aelIgnorepatPattern	String
hi def link	aelExtension		String
hi def link	aelString		String

" Identifiers
hi def link	aelGotoTarget		aelIdentifier
hi def link	aelIncludeContext	aelIdentifier
hi def link 	aelGlobalVar		aelIdentifier
hi def link	aelContextName		aelIdentifier
hi def link	aelMacroParm		aelIdentifier
hi def link	aelSetVariable		aelIdentifier
hi def link	aelInterpol		aelIdentifier
hi def link 	aelIdentifier		Identifier
hi def link	aelMacroCall		aelFunction
hi def link	aelMacroName		aelFunction
hi def link 	aelApplication		aelFunction
hi def link 	aelSet			aelFunction
hi def link 	aelFunction		Function

hi def link 	aelComment		Comment

