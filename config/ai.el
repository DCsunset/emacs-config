(use-package ellama
  :commands (make-llm-ollama)
  :config
  (require 'llm-ollama)
  (let ((coding-model (make-llm-ollama
                       :host "@OLLAMA_HOST@"
                       :port 80
		                   :chat-model "qwen2.5-coder:3b-instruct-q4_K_M"
		                   :embedding-model "nomic-embed-text"
                       :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (chat-model (make-llm-ollama
                     :host "@OLLAMA_HOST@"
                     :port 80
		                 :chat-model "qwen2.5:3b-instruct-q4_K_M"
		                 :embedding-model "nomic-embed-text"
                     :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (large-chat-model (make-llm-ollama
                           :host "@OLLAMA_HOST@"
                           :port 80
		                       :chat-model "deepseek-r1:7b"
		                       :embedding-model "nomic-embed-text"
                           :default-chat-non-standard-params '(("num_ctx" . 32768)))))
    (setopt ellama-provider chat-model)
    (setopt ellama-coding-provider coding-model)
    (setopt ellama-naming-provider chat-model)
    (setopt ellama-translation-provider large-chat-model)
    (setopt ellama-summarization-provider large-chat-model)))

