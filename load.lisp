(require :asdf)
(asdf:initialize-source-registry `(:source-registry :ignore-inherited-configuration
                                                    (:directory ,(directory-namestring *default-pathname-defaults*))))
(asdf:disable-output-translations)
(asdf:load-system "engine")
