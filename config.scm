;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
 (gnu home)
 (gnu packages)
 (gnu packages emacs)
 (gnu services)
 (guix gexp)
 (gnu home services)
 (gnu home services xdg)
 (gnu home services shells)
 (gnu home services shepherd))

(home-environment
 (packages
  (map (compose list specification->package+output)
       (list "taskwarrior"
             "emacs"
             ;; "emacs-all-the-icons"
             ;; "emacs-dockerfile-mode"
             ;; "emacs-doom-modeline"
             "emacs-evil"
             "emacs-evil-collection"
             ;; "emacs-flycheck"
             "emacs-helpful"
             ;; "emacs-lsp-mode"
             ;; "emacs-lsp-ui"
             "emacs-magit"
             ;; "emacs-mu4e-alert"
             ;; "emacs-projectile"
             ;; "emacs-no-littering"
             ;; "emacs-rustic"
             ;; "emacs-use-package"
             ;; "emacs-selectrum"
             ;; "emacs-undo-fu"
             ;; "emacs-org"
             ;; "emacs-org-superstar"
             ;; "emacs-yasnippet"
             ;; "emacs-company"
             "emacs-rust-mode"
             "emacs-which-key"
             ;; "emacs-toml-mode"
             ;; "font-fira-code"
             ;; "font-abattis-cantarell"
             "git"
             "glibc-locales"
	     "mandoc"
             "nss-certs"
             "picocom"
             ;; "rust-analyzer"
             "ripgrep"
					;"timewarrior"
             "texinfo"
             "tmux")))
 (services
  (list
   (service home-bash-service-type
	    (home-bash-configuration
	     (bash-profile
	      (list (local-file "./.bash_profile" "bash_profile")))))
   (service home-xdg-configuration-files-service-type
	    (list `("emacs/init.el" ,(local-file "emacs-init.el"))))
   (service home-shepherd-service-type
	    (home-shepherd-configuration
	     (services (list
			(shepherd-service
			 (provision '(emacs))
			 (start #~(make-forkexec-constructor
				   (list
				    #$(file-append emacs "/bin/emacs")
				    "--fg-daemon")))
			 (stop #~(make-system-destructor
				  (string-join 
				   (list
				    (string-append #$emacs "/bin/emacsclient")
				    "--eval"
				    "\"(kill-emacs)\""))))))))))))

