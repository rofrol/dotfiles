# herdr config

## macOS notifications

`config.toml` sets `[ui.toast] delivery = "system"`, so herdr shows a macOS
notification (plus sound) when an agent finishes or needs input in a
background workspace.

Without `terminal-notifier` installed, herdr delivers notifications via
`osascript`, which macOS attributes to **Script Editor**. Setup:

1. Open Script Editor once so it appears in notification settings.
2. System Settings → Notifications → Script Editor → Allow notifications
   (Desktop, alert style Temporary).
3. Focus modes silently hide banners. To still get agent notifications:
   System Settings → Focus → (each mode) → Allowed Notifications → add
   **Script Editor**.

Test:

```sh
osascript -e 'display notification "test" with title "Herdr test"'
herdr notification show "Herdr test" --body "works" --sound done
```

Apply config changes without restarting: `herdr server reload-config`
(validate first with `herdr config check`).
