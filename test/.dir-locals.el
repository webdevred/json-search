((haskell-mode
  . ((eval . (setq-local haskell-process-args-cabal-repl
                     (append '("json-search:test:json-search-test") haskell-process-args-cabal-repl))))))
