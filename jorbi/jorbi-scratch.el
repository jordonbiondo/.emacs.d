


(font-lock-add-keywords 
 'c-mode
 '(("\\(\\<[A-Z_][A-Z_0-9]+\\)\\( \\)?\\((\\)" 
    1 'font-lock-constant-face)
   ("\\(#define\\)\\( +\\)\\([A-Z_][A-Za-z0-9_]+\\)" 
    3 'font-lock-constant-face t)
   ("\\(?:ex\\(?:ec\\(?:l[ep]\\|v[Pep]\\|[lv]\\)\\|it\\)\\|fork\\|wait\\(?:pid\\)?\\)\\( *\\)\\((\\)"
    1 'font-lock-builtin-face t)))
