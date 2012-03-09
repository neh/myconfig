syn match ErrorLogFunction "console.log"

" we need the conceal feature (vim ≥ 7.3)
if has('conceal')
    " remove the keywords. we'll re-add them below
    syntax clear javaScriptFunction
    syntax match javaScriptFunction /\<function\>/ nextgroup=javaScriptFuncName skipwhite conceal cchar=ƒ
    syntax match javaScriptFunctionNoParams /function()/ conceal cchar=ƒ

    syntax match jsNiceOperator "==" conceal cchar=≡ containedin=javaScriptParen
    syntax match jsNiceOperator "===" conceal cchar=≣ containedin=javaScriptParen
    syntax match jsNiceOperator "!==" conceal cchar=≢ containedin=ALL
    syntax match jsNiceOperator "!=" conceal cchar=≠ containedin=ALL
    syntax match jsNiceOperator "<=" conceal cchar=≤ containedin=ALL
    syntax match jsNiceOperator ">=" conceal cchar=≥ containedin=ALL
    syntax match jsNiceOperator ">>" conceal cchar=» containedin=ALL
    syntax match jsNiceOperator "<<" conceal cchar=« containedin=ALL
    syntax match jsNiceOperator "\:\:" conceal cchar=∷ containedin=ALL

    hi link javaScriptFunctionNoParams javaScriptFunction
    hi link jsNiceOperator Operator
    hi link jsNiceRepeat Error
    hi! link Conceal javaScriptFunction

    set conceallevel=2
endif
