# Changelog

Toutes les modifications notables de MarkCraft sont documentées ici.  
Format basé sur [Keep a Changelog](https://keepachangelog.com/fr/1.1.0/).

## [1.5.0] — 2026-02-10

### Ajouté
- **Split redimensionnable** : poignée draggable entre éditeur et preview Markdown (ratio 20–80%)
- **Zoom ⌘+Scroll** : modifier la taille du texte avec ⌘+molette ou pinch trackpad (9–32pt)
- **Preview proportionnel** : le preview Markdown scale avec la taille de police de l'éditeur
- **Mode clair/sombre** : section Appearance dans Settings avec 3 boutons visuels (Light/Dark/System)
- **Menu View > Appearance** : sous-menu avec checkmarks pour le thème actif
- **Site web** markcraft.fr : design luxe sombre, 10 sections, responsive, i18n 10 langues
- **Politique de confidentialité** : page privacy.html pour App Store Review
- **Privacy Manifest** : PrivacyInfo.xcprivacy conforme aux exigences Apple 2024+
- **App Sandbox** : entitlements pour files, microphone, réseau, impression
- **Preview Markdown 50%** : rendu natif SwiftUI (headings, code, tables, listes, blockquotes)
- **Impression** : imprimer le preview Markdown rendu via ⌘P
- **60 exemples** : fichiers de démonstration pour chaque langage supporté
- **Documentation** : 60 fiches de langage avec historique, syntaxe, exemples
- **Journal développeur** : entrées quotidiennes, humeurs, séries, flashcards, export PDF
- **Annotations encre** : stylo, surligneur, gomme, lasso, formes, calques
- **Sessions audio** : enregistrement, timeline, transcription, effets focus
- **Terminal intégré** : historique, complétion, intégration éditeur
- **Palette de commandes** : ⌘⇧P avec recherche fuzzy
- **Mode Zen** : plein écran distraction-free avec objectifs de mots
- **Pomodoro** : minuteur intégré avec sons ambiants
- **Git status** : visualisation des changements, diff, branches
- **Minimap** : aperçu du fichier dans la gouttière
- **Coloration syntaxique** : 60 langages via moteur natif Swift
- **Numéros de ligne** : NSView overlay avec gouttière cliquable
- **Breadcrumb** : chemin de navigation fichier/dossier
- **Tabs** : barre d'onglets avec drag & drop
- **Recherche/Remplacement** : ⌘F / ⌘⇧H avec regex
- **Quick Open** : ⌘P recherche fuzzy de fichiers
- **Comparaison** : diff côte à côte entre fichiers
- **Heatmap activité** : visualisation GitHub-style des contributions
- **Confettis** : célébration des jalons (100 lignes, etc.)
- **Parallaxe** : effets visuels subtils dans l'UI
- **Export livre PDF** : journal → livre avec couverture, TdM, chapitres
- **39 langues UI** : interface traduite via MCLocalization
- **Design System** : thème cohérent avec constantes couleurs
- **Test Framework** : framework de tests léger intégré

### Corrigé
- Bundle ID corrigé : `fr.markcraft.app` (au lieu de `com.markcraft.app`)
- Email presse : `presse@markcraft.fr` (au lieu de `press@markcraft.fr`)
- GitHub URL : `github.com/markcraft-app/markcraft`
- Lignes de code : compteur mis à jour (13 019 lignes)

## [1.0.0] — 2026-01-15

### Ajouté
- Version initiale de MarkCraft
- Éditeur de code SwiftUI natif
- Coloration syntaxique basique
- Thème sombre par défaut

[1.5.0]: https://github.com/markcraft-app/markcraft/compare/v1.0.0...v1.5.0
[1.0.0]: https://github.com/markcraft-app/markcraft/releases/tag/v1.0.0
