(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-to-list 'company-backends 'company-jedi)
