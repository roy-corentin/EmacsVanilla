;;; cr-corfu-icons.el --- corfu custom icon setup -*- lexical-binding: t; -*-
;;; Commentary:


;;; Code:

(defvar cr/custom-corfu-icons
  '((array :style "md" :icon "abjad_arabic" :face font-lock-type-face)
    (boolean :style "md" :icon "ideogram_cjk" :face font-lock-builtin-face)
    (class :style "md" :icon "syllabary_katakana" :face font-lock-type-face)
    (color :style "md" :icon "palette" :face success)
    (command :style "md" :icon "console" :face default)
    (constant :style "md" :icon "zodiac_pisces" :face font-lock-constant-face)
    (constructor :style "md" :icon "hammer_wrench" :face font-lock-function-name-face)
    (enummember :style "md" :icon "numeric_1_box" :face font-lock-builtin-face)
    (enum-member :style "md" :icon "numeric_2_box" :face font-lock-builtin-face)
    (enum :style "md" :icon "format_list_numbered" :face font-lock-builtin-face)
    (event :style "md" :icon "bell_ring" :face font-lock-warning-face)
    (field :style "md" :icon "syllabary_hiragana" :face font-lock-variable-name-face)
    (file :style "md" :icon "file_document" :face font-lock-string-face)
    (folder :style "md" :icon "folder_open" :face font-lock-doc-face)
    (interface :style "md" :icon "alphabet_greek" :face font-lock-type-face)
    (keyword :style "md" :icon "key_variant" :face font-lock-keyword-face)
    (macro :style "md" :icon "script_text" :face font-lock-keyword-face)
    (magic :style "md" :icon "creation" :face font-lock-builtin-face)
    (method :style "md" :icon "furigana_vertical" :face font-lock-function-name-face)
    (function :style "md" :icon "furigana_horizontal" :face font-lock-function-name-face)
    (module :style "md" :icon "package_variant" :face font-lock-preprocessor-face)
    (numeric :style "md" :icon "numeric" :face font-lock-builtin-face)
    (operator :style "md" :icon "omega" :face font-lock-comment-delimiter-face)
    (param :style "md" :icon "alphabet_cyrillic" :face default)
    (property :style "md" :icon "tag" :face font-lock-variable-name-face)
    (reference :style "md" :icon "book_open_variant" :face font-lock-variable-name-face)
    (snippet :style "md" :icon "odnoklassniki" :face font-lock-string-face)
    (string :style "md" :icon "format_quote_close" :face font-lock-string-face)
    (struct :style "md" :icon "cube_outline" :face font-lock-variable-name-face)
    (text :style "md" :icon "alphabet_latin" :face font-lock-doc-face)
    (typeparameter :style "md" :icon "alphabet_tengwar" :face font-lock-type-face)
    (type-parameter :style "md" :icon "alphabet_aurebesh" :face font-lock-type-face)
    (unit :style "md" :icon "ruler" :face font-lock-constant-face)
    (value :style "md" :icon "alpha_v_box" :face font-lock-builtin-face)
    (variable :style "md" :icon "abugida_devanagari" :face font-lock-variable-name-face)
    (t :style "md" :icon "compass_rose" :face font-lock-warning-face)))

(provide 'cr-corfu-icons)
;;; cr-corfu-icons.el ends here
