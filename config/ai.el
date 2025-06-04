(use-package ellama
  :commands (make-llm-ollama)
  :config
  (require 'llm-ollama)
  (let ((coding-model (make-llm-ollama
                       :host "@OLLAMA_HOST@"
                       :port 80
		                   :chat-model "@OLLAMA_CODING_MODEL@"
		                   :embedding-model "nomic-embed-text"
                       :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (chat-model (make-llm-ollama
                     :host "@OLLAMA_HOST@"
                     :port 80
		                 :chat-model "@OLLAMA_CHAT_MODEL@"
		                 :embedding-model "nomic-embed-text"
                     :default-chat-non-standard-params '(("num_ctx" . 32768)))))
    (setopt ellama-provider chat-model)
    (setopt ellama-coding-provider coding-model)
    (setopt ellama-naming-provider chat-model)
    (setopt ellama-translation-provider chat-model)
    (setopt ellama-summarization-provider chat-model)))

