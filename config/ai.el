(use-package ellama
  :commands (make-llm-openai-compatible)
  :config
  (require 'llm-openai)
  (let ((coding-model (make-llm-openai-compatible
                       :url "@AI_URL@"
                       :key "@AI_API_KEY@"
		                   :chat-model "@AI_CODING_MODEL@"
		                   :embedding-model "@AI_EMBEDDING_MODEL"
                       :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (chat-model (make-llm-openai-compatible
                     :url "@AI_URL@"
                     :key "@AI_API_KEY@"
		                 :chat-model "@AI_CHAT_MODEL@"
		                 :embedding-model "@AI_EMBEDDING_MODEL"
                     :default-chat-non-standard-params '(("num_ctx" . 32768)))))
    (setopt ellama-provider chat-model)
    (setopt ellama-coding-provider coding-model)
    (setopt ellama-naming-provider chat-model)
    (setopt ellama-translation-provider chat-model)
    (setopt ellama-summarization-provider chat-model)))

(use-package minuet
  :bind
  (:map minuet-active-mode-map
        ("M-p" . minuet-previous-suggestion)
        ("M-n" . minuet-next-suggestion)
        ("TAB" . minuet-accept-suggestion)
        ("<tab>" . minuet-accept-suggestion)
        ;; accept first N lines (by prefix)
        ("S-TAB" . minuet-accept-suggestion-line)
        ("S-<tab>" . minuet-accept-suggestion-line)
        ("C-c" . minuet-dismiss-suggestion))
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
  (setq minuet-context-window 1024)
  (plist-put minuet-openai-fim-compatible-options :end-point "@AI_URL@/completions")
  (plist-put minuet-openai-fim-compatible-options :name "AI")
  (plist-put minuet-openai-fim-compatible-options :api-key (lambda () "@AI_API_KEY@"))
  (plist-put minuet-openai-fim-compatible-options :model "@AI_CODING_MODEL@")
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 256))

(modaled-define-keys
  :states '("insert")
  :bind
  `(("M-o" . ("ai complete" . minuet-show-suggestion))))


