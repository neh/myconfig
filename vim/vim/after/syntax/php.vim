syn keyword ErrorLogFunction error_log containedin=phpRegion contained
syn keyword ErrorLogFunction var_dump containedin=phpRegion contained
syn keyword ErrorLogFunction print_r containedin=phpRegion contained

if !exists('g:no_php_conceal') && has('conceal') && &enc == 'utf-8'
    syntax match phpNiceOperator "<-" conceal cchar=← containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "->" conceal cchar=→ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "==" conceal cchar=≡ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "===" conceal cchar=≡ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "!=" conceal cchar=≠ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "!==" conceal cchar=≠ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator ">>" conceal cchar=» containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "<=" conceal cchar=≲ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator ">=" conceal cchar=≳ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "=>" conceal cchar=⇒ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "\:\:" conceal cchar=∷ containedin=phpRegion,@phpClConst contained
    syntax match phpNiceOperator "\.\." conceal cchar=‥ containedin=phpRegion,@phpClConst contained

    " can't get these ones to work. function will only be concealed if it's 
    " outside a class.
    "syntax match phpNiceRepeat "\<foreach\>" conceal cchar=∀ containedin=phpRegion contained
    "syntax match phpNiceDefine "\<function\>" conceal cchar=ƒ containedin=phpRegion contained

    hi link phpNiceOperator Operator
    "hi link phpNiceRepeat Repeat
    "hi link phpNiceDefine Define
    hi! link Conceal Operator
    setlocal conceallevel=2
endif
