# pushover-client

Receive Pushover notifications via the [Pushover Open Client API](https://pushover.net/api/client)

## Installation

1. Have [stack](https://docs.haskellstack.org/en/stable/README/)
1. Have `~/.local/bin` in your `$PATH`
1. Clone & install:

   ```console
   git clone https://github.com/pbrisbin/pushover-client
   cd pushover-client
   stack setup
   stack install
   ```

## Usage

```console
pushover-client register "you@example.com" "your-password" "unique-device-name"
```

```console
pushover-client receive
```

## Examples

There is only one output format, JSON:

```json
[
  {
    "priority": 0,
    "icon": "pushover",
    "app": "Pushover",
    "date": 1504875037,
    "aid": 1,
    "umid": 9658,
    "id": 2,
    "acked": 0,
    "title": "Welcome to Pushover!",
    "message": "This device (...) is now able to receive notifications and your 7-day trial has started.\n\nVisit https://pushover.net/apps to view apps, plugins, and services to use with Pushover just by supplying your user key:\n\n..."
  },
  {
    "priority": 0,
    "icon": "pushover",
    "app": "Pushover",
    "date": 1504875304,
    "aid": 1,
    "umid": 9661,
    "id": 3,
    "acked": 0,
    "title": "Test message",
    "message": "This is a test of the messaging system."
  },
  {
    "priority": -2,
    "icon": "jWsFAyQHEjs4nmt",
    "app": "sabnzbd",
    "date": 1504890000,
    "aid": 142534,
    "umid": 9663,
    "id": 5,
    "acked": 0,
    "title": "Added NZB",
    "message": "Paused"
  }
]
```

For further processing or interrogation, I recommend [jq](https://stedolan.github.io/jq/).

Examples:

```sh
# Pretty-print
pushover-client receive | jq .

# Show just message(s)
pushover-client receive | jq '.[] | .message'

# Check if you have a specific kind of message
pushover-client receive \
  | jq --raw-output '.[] | .app' \
  | grep -Fiq sickbeard \
  && echo "new content downloaded, trigger video library update..."
```

## Development & Test

```
stack setup
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
