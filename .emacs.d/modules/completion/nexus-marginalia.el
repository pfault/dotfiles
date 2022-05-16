(use-package marginalia
    :after selectrum
    :demand
    :init
    (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :config (marginalia-mode t))
