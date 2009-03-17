" Vim color file
" Maintainer:	Lukas Zapletal <Lukas.Zapletal@seznam.cz>
" Version: 1.2
" Last Change:	2001 Jul 15

" This is the color scheme for Embperl like Interdev (yellow code).
" To use it use command like this:
"
" autocmd BufNewFile,BufRead *.epl,*.phtml colorscheme embperl_yellow
"
" CHANGELOG: v1.0 - initial release
" v1.2	- major bugfix with background in perl statements
"
" TODO:
" - hold the color in a variable to let user change it`s value
"  (I do not know how to store color #xxxxxx in a variable)

highlight clear Normal
set bg&
hi clear
if exists("syntax_on")
  syntax reset
endif

" create special "Embperl" constants
if &background == "dark"
  hi embpComment	term=bold cterm=NONE ctermfg=Cyan ctermbg=NONE gui=NONE guifg=#80a0ff guibg=#ffffdd
  hi embpConstant	term=underline cterm=NONE ctermfg=Magenta ctermbg=NONE gui=NONE guifg=#ffa0a0 guibg=#ffffdd
  hi embpSpecial	term=bold cterm=NONE ctermfg=LightRed ctermbg=NONE gui=NONE guifg=Orange guibg=#ffffdd
  hi embpIdentifier	term=underline cterm=bold ctermfg=Cyan ctermbg=NONE gui=NONE guifg=#40ffff guibg=#ffffdd
  hi embpStatement	term=bold cterm=NONE ctermfg=Yellow ctermbg=NONE gui=bold guifg=#ffff60 guibg=#ffffdd
  hi embpPreProc	term=underline cterm=NONE ctermfg=LightBlue ctermbg=NONE gui=NONE guifg=#ff80ff guibg=#ffffdd
  hi embpType		term=underline cterm=NONE ctermfg=LightGreen ctermbg=NONE gui=bold guifg=#60ff60 guibg=#ffffdd
  hi embpIgnore		term=NONE cterm=NONE ctermfg=black ctermbg=NONE gui=NONE guifg=bg guibg=#ffffdd
else
  hi embpComment	term=bold cterm=NONE ctermfg=DarkBlue ctermbg=NONE gui=NONE guifg=Blue guibg=#ffffdd
  hi embpConstant	term=underline cterm=NONE ctermfg=DarkRed ctermbg=NONE gui=NONE guifg=Magenta guibg=#ffffdd
  hi embpSpecial	term=bold cterm=NONE ctermfg=DarkMagenta ctermbg=NONE gui=NONE guifg=SlateBlue guibg=#ffffdd
  hi embpIdentifier	term=underline cterm=NONE ctermfg=DarkCyan ctermbg=NONE gui=NONE guifg=DarkCyan guibg=#ffffdd
  hi embpStatement	term=bold cterm=NONE ctermfg=Brown ctermbg=NONE gui=bold guifg=Brown guibg=#ffffdd
  hi embpPreProc	term=underline cterm=NONE ctermfg=DarkMagenta ctermbg=NONE gui=NONE guifg=Purple guibg=#ffffdd
  hi embpType		term=underline cterm=NONE ctermfg=DarkGreen ctermbg=NONE gui=bold guifg=SeaGreen guibg=#ffffdd
  hi embpIgnore		term=NONE cterm=NONE ctermfg=white ctermbg=NONE gui=NONE guifg=bg guibg=#ffffdd
endif
hi embpError	term=reverse cterm=NONE ctermfg=White ctermbg=Red gui=NONE guifg=White guibg=Red
hi embpTodo		term=standout cterm=NONE ctermfg=Black ctermbg=Yellow gui=NONE guifg=White guibg=Black

hi EmbperlInsideHtml guibg=#ffffdd

hi link embpString		embpConstant
hi link embpCharacter	embpConstant
hi link embpNumber		embpConstant
hi link embpBoolean		embpConstant
hi link embpFloat		embpNumber
hi link embpFunction	embpIdentifier
hi link embpConditional	embpStatement
hi link embpRepeat		embpStatement
hi link embpLabel		embpStatement
hi link embpOperator	embpStatement
hi link embpKeyword		embpStatement
hi link embpException	embpStatement
hi link embpInclude		embpPreProc
hi link embpDefine		embpPreProc
hi link embpMacro		embpPreProc
hi link embpPreCondit	embpPreProc
hi link embpStorageClass	embpType
hi link embpStructure	embpType
hi link embpTypedef		embpType
hi link embpTag		embpSpecial
hi link embpSpecialChar	embpSpecial
hi link embpDelimiter	embpSpecial
hi link embpSpecialComment	embpSpecial
hi link embpDebug		embpSpecial

if version >= 508
  if version < 508
    command -nargs=+ HiLink hi! link <args>
  else
    command -nargs=+ HiLink hi! def link <args>
  endif
  " The default highlighting.
  HiLink perlSharpBang		embpPreProc
  HiLink perlControl		embpPreProc
  HiLink perlInclude		embpInclude
  HiLink perlSpecial		embpSpecial
  HiLink perlString		embpString
  HiLink perlCharacter		embpCharacter
  HiLink perlNumber		embpNumber
  HiLink perlFloat		embpFloat
  HiLink perlType		embpType
  HiLink perlIdentifier		embpIdentifier
  HiLink perlLabel		embpLabel
  HiLink perlStatement		embpStatement
  HiLink perlConditional	embpConditional
  HiLink perlRepeat		embpRepeat
  HiLink perlOperator		embpOperator
  HiLink perlFunction		embpFunction
  HiLink perlFunctionPrototype	embpperlFunction
  HiLink perlComment		embpComment
  HiLink perlTodo		embpTodo
  HiLink perlStringStartEnd	perlString
  HiLink perlList		perlStatement
  HiLink perlMisc		perlStatement
  HiLink perlVarPlain		perlIdentifier
  HiLink perlFiledescRead	perlIdentifier
  HiLink perlFiledescStatement	perlIdentifier
  HiLink perlVarSimpleMember	perlIdentifier
  HiLink perlVarSimpleMemberName perlString
  HiLink perlVarNotInMatches	perlIdentifier
  HiLink perlVarSlash		perlIdentifier
  HiLink perlQQ			perlString
  if version >= 600
    HiLink perlHereDoc		perlString
  else
    HiLink perlHereIdentifier	perlStringStartEnd
    HiLink perlUntilEOFDQ	perlString
    HiLink perlUntilEOFSQ	perlString
    HiLink perlUntilEmptyDQ	perlString
    HiLink perlUntilEmptySQ	perlString
    HiLink perlUntilEOF		perlString
  endif
  HiLink perlStringUnexpanded	perlString
  HiLink perlSubstitutionSQ	perlString
  HiLink perlSubstitutionDQ	perlString
  HiLink perlSubstitutionSlash	perlString
  HiLink perlSubstitutionHash	perlString
  HiLink perlSubstitutionBracket perlString
  HiLink perlSubstitutionCurly	perlString
  HiLink perlSubstitutionPling	perlString
  HiLink perlTranslationSlash	perlString
  HiLink perlTranslationHash	perlString
  HiLink perlTranslationBracket	perlString
  HiLink perlTranslationCurly	perlString
  HiLink perlMatch		perlString
  HiLink perlMatchStartEnd	perlStatement
  HiLink perlFormatName		perlIdentifier
  HiLink perlFormatField	perlString
  HiLink perlPackageDecl	perlType
  HiLink perlStorageClass	perlType
  HiLink perlPackageRef		perlType
  HiLink perlStatementPackage	perlStatement
  HiLink perlStatementSub	perlStatement
  HiLink perlStatementStorage	perlStatement
  HiLink perlStatementControl	perlStatement
  HiLink perlStatementScalar	perlStatement
  HiLink perlStatementRegexp	perlStatement
  HiLink perlStatementNumeric	perlStatement
  HiLink perlStatementList	perlStatement
  HiLink perlStatementHash	perlStatement
  HiLink perlStatementIOfunc	perlStatement
  HiLink perlStatementFiledesc	perlStatement
  HiLink perlStatementVector	perlStatement
  HiLink perlStatementFiles	perlStatement
  HiLink perlStatementFlow	perlStatement
  HiLink perlStatementScope	perlStatement
  HiLink perlStatementInclude	perlStatement
  HiLink perlStatementProc	perlStatement
  HiLink perlStatementSocket	perlStatement
  HiLink perlStatementIPC	perlStatement
  HiLink perlStatementNetwork	perlStatement
  HiLink perlStatementPword	perlStatement
  HiLink perlStatementTime	perlStatement
  HiLink perlStatementMisc	perlStatement
  HiLink perlFunctionName	perlIdentifier
  HiLink perlFunctionPRef	perlType
  HiLink perlPOD		perlComment
  HiLink perlShellCommand	perlString
  HiLink perlSpecialAscii	perlSpecial
  HiLink perlSpecialDollar	perlSpecial
  HiLink perlSpecialString	perlSpecial
  HiLink perlSpecialStringU	perlSpecial
  HiLink perlSpecialMatch	perlSpecial
  HiLink perlSpecialBEOM	perlSpecial
  HiLink perlDATA		perlComment

  HiLink perlBrackets		embpError

  " Possible errors
  HiLink perlNotEmptyLine	embpError
  HiLink perlElseIfError	embpError

  delcommand HiLink
endif

let colors_name = "embperl_yellow"

" vim: sw=2
