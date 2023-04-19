(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(better-jumper-add-jump-behavior 'replace)
 '(ede-project-directories '("/home/cguidi/uftrace"))
 '(org-agenda-files
   '("~/Documents/design" "/home/cguidi/org/projects.org" "/home/cguidi/org/todo.org"))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "make DEBUG=1 -j")
     (projectile-project-run-cmd . "make -j runtest")
     (projectile-project-run-cmd . "make runtest")
     (projectile-project-install-cmd . "sudo make install")
     (projectile-project-test-cmd . "make -j unittest")
     (projectile-project-compilation-cmd . "make -j")
     (projectile-project-configure-cmd . "./configure")
     (flycheck-gcc-include-path "/home/cguidi/uftrace" "/home/cguidi/uftrace/arch/x86_64")
     (flycheck-gcc-include-path quote
      ("/home/cguidi/uftrace" "/home/cguidi/uftrace/arch/x86_64")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
