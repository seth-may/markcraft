# Contribuer à MarkCraft

Merci de votre intérêt pour MarkCraft ! Voici comment contribuer.

## Prérequis

- macOS 14 Sonoma ou supérieur
- Xcode 15.0+
- Swift 5.9+

## Configuration

```bash
git clone https://github.com/markcraft-app/markcraft.git
cd markcraft
open MarkCraft.xcodeproj
```

Sélectionnez le scheme **MarkCraft** et cliquez ▶️ Run.

## Structure du projet

```
MarkCraft/
├── MarkCraftApp_1_MarkCraftApp.swift   # Point d'entrée, menus, commandes
├── AppState_1_AppState.swift           # État global de l'application
├── RootView_1_RootView.swift           # Vue racine avec split view
├── SyntaxEngine_1_SyntaxEngine.swift   # Moteur de coloration syntaxique
├── Documentation_1_Documentation.swift  # 60 fiches de langage
├── EditorAdvanced_1_EditorAdvanced.swift # Preview Markdown, bracket match
├── JournalSystem_1_JournalSystem.swift  # Journal développeur
├── MCLocalization.swift                 # 39 langues UI
└── ...                                  # 41 fichiers Swift au total
```

## Conventions

- **Swift** : SwiftUI natif, pas de UIKit/AppKit sauf NSTextView pour l'éditeur
- **Nommage** : `GroupName_N_Description.swift`
- **Commits** : messages en anglais, présent impératif (`Add feature`, `Fix bug`)
- **Style** : 4 espaces, pas de trailing whitespace

## Pull Requests

1. Créez une branche depuis `main`
2. Faites vos modifications
3. Vérifiez que le projet compile sans warning
4. Soumettez votre PR avec une description claire

## Signaler un bug

Utilisez les [Issues GitHub](https://github.com/markcraft-app/markcraft/issues) avec :
- Version de macOS
- Description du problème
- Étapes pour reproduire
- Capture d'écran si possible

## Code de conduite

Soyez respectueux. Aucune forme de harcèlement ou discrimination ne sera tolérée.

## Licence

En contribuant, vous acceptez que vos contributions soient cédées à l'auteur et soumises à la licence propriétaire de MarkCraft (voir LICENSE).
