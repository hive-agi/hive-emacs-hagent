# hive-emacs-hagent

**Headed Agent** chat backends for Emacs — written in [cljel](https://github.com/hive-agi/clojure-elisp).

Lean alternative to `claude-code-ide.el`. Provides chat buffers backed by headed CLI agents:

| Backend | CLI | Status |
|---|---|---|
| `openclaude` | [`openclaude`](https://github.com/Gitlawb/openclaude) | alpha |
| `agent` | Claude Agent SDK CLI | planned |

Everything speaks a single `IChatBackend` protocol so adding new headed agents is a ~200 LOC adapter.

## Install

Requires: Emacs 29+, [`clel`](https://github.com/hive-agi/clojure-elisp) compiler, `clj` on PATH.

```bash
git clone https://github.com/hive-agi/hive-emacs-hagent ~/.emacs.d/hive-emacs-hagent
cd ~/.emacs.d/hive-emacs-hagent
./build.sh          # compiles src/cljel → elisp/
```

Then in `init.el`:

```elisp
(add-to-list 'load-path "~/.emacs.d/hive-emacs-hagent/elisp")
(require 'hive-hagent)
(global-set-key (kbd "C-c h a") #'hive-hagent)
```

## Develop

Interactive cljel development via CIDER + cljel nREPL middleware:

```bash
clj -M:dev -m clojure-elisp.nrepl.server 7921
```

Connect CIDER to port 7921; evaluate forms in `src/cljel/hive_hagent/*.cljel` — they recompile and reload in the running Emacs over the nREPL bridge.

## Architecture

```
┌─────────────────────────┐
│   hive-hagent-mode      │   minor mode + transient UI
├─────────────────────────┤
│   hive-hagent.core      │   chat buffer, stream renderer
├─────────────────────────┤
│   IChatBackend          │   protocol (start/send/interrupt/stop)
├──────────┬──────────────┤
│ openclaude│    agent    │   adapters (subprocess + stream parser)
└──────────┴──────────────┘
```

Design notes live in `doc/` (TBD). For the EPICs driving this repo see hive kanban tasks `20260414121731-43820db1` and `20260414121734-2ca1df76`.

## License

MIT — see `LICENSE`.
