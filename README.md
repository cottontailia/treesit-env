# treesit-env.el

[![License: CC0](https://img.shields.io/badge/License-CC0-blue.svg)](http://creativecommons.org/publicdomain/zero/1.0/)
[![Emacs](https://img.shields.io/badge/Emacs-29.1+-purple.svg)](https://www.gnu.org/software/emacs/)

`treesit-env` is a package for declarative and centralized management of the built-in Tree-sitter (`treesit.el`) environment.

## ✨ Features
- **Declarative Configuration**: Define languages, repositories, and major-mode mappings using a single DSL. It respects the intuitive design of `use-package`, the gold standard for Emacs interfaces.
- **🔗 Dependency Resolution**: Automatically handles the installation order for complex grammars, such as `typescript` and `tsx`.
- **🛠️ Automatic ABI Compatibility**: Automatically determines the ABI version supported by your Emacs and traverses the Git history to find a compatible revision. It solves the "grammar is too new for Emacs" issue with brute force.
- **🚀 On-demand Installation**: It doesn't download everything upfront. Grammars are installed **on the fly** only when you open a file for which the grammar is missing.
- **⚡ Flexible Compiler Settings**: Change compilers freely. For example, by specifying `zig cc`, you can compile grammars without needing a heavy toolchain.
- **🪶 (Supposedly) Lightweight**: While Tree-sitter related packages often feel heavy for some reason, this package prioritizes performance by avoiding excessive feature bloat. It aims to let you build your environment as naturally and lightly as possible... hopefully.

## 💡 Background: Why this package?
Since `treesit.el` debuted in Emacs 29.1, users have hit several **pain points**:

1. **Fragmented Configuration**: Download settings, mode mappings, and installation methods are often scattered. This package consolidates them into a single declaration.
2. **Complex Dependencies**: When a language requires another grammar (e.g., CPP for C), it is rarely explicit, making manual installation order a headache.
3. **The "Windows Wall"**: Compiling grammars on Windows usually requires massive environments like MSYS2 or MinGW, which is a high hurdle for many.
4. **Lack of Shared Recipes**: Correct repository URLs and configurations aren't well-organized in the community, causing many users to give up on Tree-sitter.

`treesit-env` aims to solve these issues through "declarative recipe management" and "flexible compilation support."

## 🧩 ABI Compatibility & Revision Management
Tree-sitter grammars and Emacs are currently in a transition period, leading to frequent "ABI mismatch" errors. `treesit-env` provides three strategies to solve this:

1. **Auto Discovery (`auto`)**: During installation, it traverses the Git history to find the latest revision compatible with the ABI supported by your current Emacs.
2. **Fixed (`"v0.20.1"`)**: Specify a specific tag or commit. This is the fastest and most stable as no discovery is required.
3. **Fetch Latest (Default)**: The standard behavior when `:revision` is omitted. Always fetches the latest master/main, though it carries a risk of ABI mismatch.

## 🪟 Tree-sitter on Windows: Difficulties & Solutions
Typically, using Tree-sitter on Windows requires installing MSYS2 or MinGW. These are multi-gigabyte environments that can cause path conflicts and runtime dependency issues.

This package recommends using [Zig](https://ziglang.org/) as a compiler, especially on Windows. Zig is a tiny, single binary that includes an excellent C/C++ compiler.

1. Download Zig from the [official page](https://ziglang.org/download/) and add it to your PATH.
2. Set `treesit-env-compiler-cc` and `treesit-env-compiler-c++` to use Zig (see the example in the [Installation](#installation) section).

With just this, you can stably compile any grammar on the fly without a bloated environment.

## 📦 Installation & Setup

### <div id="installation">1. Installation</div>
Using the `:vc` keyword in `use-package` is recommended:

```elisp
(use-package treesit-env
  :vc (:url "https://github.com/cottontailia/treesit-env")
  :custom
  ;; Using Zig as a compiler (highly recommended for Windows)
  (treesit-env-compiler-cc '("zig" "cc"))
  (treesit-env-compiler-c++ '("zig" "c++"))

  ;; Determines the default behavior when :revision is omitted:
  ;; nil (Default): Fetches the latest master/main
  ;; t: Behaves as `auto` (Automatic ABI discovery)
  (treesit-env-default-revision-auto t)

  ;; Max ABI version your Emacs can interpret
  ;; Adjust or remove this once ABI 15+ stabilizes and Emacs supports it
  (treesit-env-abi-max 14)

  :config
  ;; Use the provided minimal sample recipes (Optional)
  (require 'treesit-env-recipe-placeholder)
  (treesit-env-source treesit-env-recipe-placeholder))
```

### 2. Enabling Languages
Settings become active once registered with the `treesit-env` macro. **The actual grammar installation occurs only when you open a file with the corresponding extension or major mode.**

#### Individual Activation
```elisp
;; Example: Add to your `use-package` :config block (for any package)
(use-package treesit-env
  :config
  ;; Simply list language names if recipes exist
  (treesit-env python rust toml)

  ;; Override or add specific settings for an existing recipe
  ;; Example: Custom extension for the javascript recipe
  (treesit-env javascript :mode "\\.jslib\\'" "\\.mjs\\'")

  ;; Specify a known revision to skip history discovery and speed up setup
  (treesit-env go :revision "v0.21.0"))
```

#### Manual Configuration (No pre-defined recipe)
Example: `markdown-ts-mode` is not built-in (you need to install the major-mode via `package.el`), but you can define its grammar setup like this:

```elisp
;; Example: Defining and enabling a grammar within a :config block
(use-package treesit-env
  :config
  (treesit-env markdown
    :vc grammars
    :mode "\\.md\\'" gfm-mode))
```

#### <div id="keywords">🔑 `treesit-env` Keywords</div>

| Keyword | Description |
| :--- | :--- |
| `:use` | Name of the `ts-mode` to use. Defaults to `(lang)-ts-mode`. |
| `:vc` | Source repository for the grammar. See the patterns below. |
| `:revision` | Git revision (tag, branch, or commit hash). Use `auto` for discovery. **Defaults to the value of `treesit-env-default-revision-auto` if omitted.** |
| `:src-path` | Subdirectory containing the source (for monorepos). |
| `:deps` | List of languages that must be installed/compiled first. |
| `:mode` | Regex for file extensions or the original major-mode to remap. **Can be multiple.** |
| `:interpreter` | Settings for the shebang (`#!`). **Can be multiple.** |

**`:vc` Patterns**

| Value | Behavior / Expanded URL |
| :--- | :--- |
| (Omitted) | Uses the official `tree-sitter/tree-sitter-(lang)`. |
| `grammars` | Uses the `tree-sitter-grammars` organization repository. |
| `"user/repo"` | Uses the specific GitHub `user/repo`. |
| `"https://..."` | Uses the full URL (GitLab, Codeberg, mirrors, etc.). |

#### 💡 Pro Tip: Customizing Mode Behavior
`treesit-env` focuses exclusively on grammar infrastructure management and does not provide features to adjust major-mode behaviors, such as indentation.

If you want to customize a specific `-ts-mode`, it may be better to include the `treesit-env` macro call within that mode's own `use-package` block. This keeps all settings related to that language in one place.

In this case, there are three important configuration requirements:

1. **Execution Timing**: The `treesit-env` macro must be executed before the mode starts, so **it must be placed in the `:init` block.**
2. **Load Order & Warning Suppression**: Many `-ts-mode` packages check for the existence of the library the moment they are loaded and will display a massive wall of warnings if it is missing. To prevent this and allow on-demand installation to function smoothly, it is **necessary to specify `:defer t`** to avoid immediate loading.
3. **Package Name Mismatch**: Be aware that the package name may differ from the mode name (e.g., `js-ts-mode` is provided by the `js` package). Ensure you use the correct name for `use-package`.

General settings like indentation can then be handled as usual in `:config` (or `:custom`).

```elisp
;; Example 1: Configuring js-ts-mode (Package name is `js`)
(use-package js
  ;; Deferred loading is recommended to prevent warnings during library checks at load time.
  :defer t
  :init
  ;; Since `js-ts-mode` is built into `js.el`, the package name is `js`.
  ;; The macro MUST be in :init to ensure it is evaluated before the mode loads.
  (treesit-env javascript :mode "\\.jslib\\'" "\\.mjs\\'")

  :config
  ;; Adjust major-mode specific behaviors in the standard configuration blocks.
  (setq js-indent-level 2))

;; Example 2: php-ts-mode (Package name is also `php-ts-mode`)
(use-package php-ts-mode
  :defer t
  :init
  ;; By registering `treesit-env` here, installation will be prompted upon
  ;; the first file open, and the mode will start safely once completed.
  (treesit-env php)
  :config
  (setq php-indent-level 2))
```

### <div id="batch-activation">⚠️ Bulk Activation</div>
```elisp
(treesit-env-all)
```

> [!NOTE]
> This operation attempts to remap all languages in the recipes to `-ts-mode`. Since this prevents fine-grained control, it is generally recommended to use the `treesit-env` macro to enable only what you need.

## 🛠️ On Recipe Management
**A Note from the Author**:  
I have no desire to be the lifelong **caretaker** of a recipe database. I’d much rather spend my time writing creative code than performing the sterile task of manually maintaining a list of URLs. Therefore, I generally do not accept Pull Requests for adding or fixing recipes.

Instead, I've provided a **Dump feature** (`M-x treesit-env-dump-recipes`). It generates recipe data including all modifications made via macros. You can paste this into your `init.el` or share it with others. My hope is that users will share recipes with each other, forming an autonomous ecosystem that does not depend on a single maintainer.

> [!IMPORTANT]
> **Please refrain from sending recipe additions or your dumped configurations to me personally.**

## 🔍 Macro, Function & Command Reference

### Macro
| Name | Description |
| :--- | :--- |
| `treesit-env` | Activates languages and overrides settings. Supports [keywords](#keywords). |
| `treesit-env-recipes` | Generates a list of recipes from DSL-style definitions. |

### Function
| Name | Description |
| :--- | :--- |
| `treesit-env-source` | Registers a list of recipes into the internal database. |
| `treesit-env-all` | Activates all languages in the recipes. Can also be called as a command. See [Bulk Activation](#batch-activation) for details. |

### Command (M-x)
| Name | Description |
| :--- | :--- |
| `treesit-env-install-all` | Installs all missing grammars for active languages at once. |
| `treesit-env-reinstall` | Force-deletes and re-installs/re-compiles a specific grammar. |
| `treesit-env-dump-recipes` | Dumps current active configurations in recipe format. |
| `treesit-env-reset-skips` | Clears all "don't ask again" flags for skipped installations. |

## 📜 License
This project is published under **CC0 (Public Domain)**.

This package was created with the support of AI, guided by the author's trial and error and design intent. Given the collaborative nature with AI, strictly asserting copyright would involve **excessive and tedious administrative effort**. Thus, CC0 was chosen as a pragmatic decision.
