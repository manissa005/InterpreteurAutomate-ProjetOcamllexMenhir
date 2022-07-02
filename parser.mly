
%token LPAREN RPAREN
%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS
%token SEMICOLON COMMA EOF
%token <Type.lettre> LETTRE
%start <Type.automate> s
%{ open Type %}
%%

s: d=declarations t=transitions EOF                                                                      {(d,t)}

declarations: is=inputsymbols ss=stacksymbols s=states istate=initialstate istack=initialstack           {(is,ss,s,istate,istack)}

inputsymbols: INPUTSYMBOLS slnv=suitelettresnonvide                                                      {slnv}

stacksymbols: STACKSYMBOLS slnv=suitelettresnonvide                                                      {slnv}

states: STATES slnv=suitelettresnonvide                                                                  {slnv}

initialstate: INITIALSTATE l=LETTRE                                                                      {l}

initialstack: INITIALSTACK l=LETTRE                                                                      {l}

suitelettresnonvide: 
|l=LETTRE                                                                                                {Element(l)}
|l=LETTRE COMMA slnv=suitelettresnonvide                                                                 {List(l,slnv)}

transitions: TRANSITIONS trl=translist                                                                   {trl}

translist: 
|tr=transition trl=translist                                                                             {Transitions(tr,trl)}
|                                                                                                        {None2}

transition: LPAREN l1=LETTRE COMMA lov=lettreouvide COMMA l2=LETTRE COMMA l3=LETTRE COMMA s=stack RPAREN {(l1,lov,l2,l3,s)} 

lettreouvide: 
|l=LETTRE                                                                                                {Lettre(l)}
|                                                                                                        {None}

stack: 
|nes=nonemptystack                                                                                       {Stack(nes)}
|                                                                                                        {None1}

(*nonemptystack:
|l1=LETTRE                                                                                               {Lettre1(l1)}
|l2=LETTRE SEMICOLON nes=nonemptystack                                                                   {StackNonVide(l2,nes)}*)

nonemptystack:
|l1=LETTRE                                                                                               {[l1]}
|l2=LETTRE SEMICOLON nes=nonemptystack                                                                   {l2::nes}