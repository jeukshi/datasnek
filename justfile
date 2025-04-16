_default:
  @just --list

# Build package.
build name='all':
    cabal build {{name}}

# Test package.
test name='all':
    cabal test {{name}} --test-show-details=direct

format:
    fourmolu -i src

# run in ghcid
dev:
	ghcid --warnings --test "main"

run executable='datasnek':
    cabal run {{executable}}

web:
    ghcid --warnings --test Main.main --command "cabal repl --enable-multi-repl exe:datasnek lib:datasnek"

send-settings:
	curl -X POST http://localhost:3000/api/settings \
	     -H "Content-Type: application/json" \
	     -H "Cookie: datasnek-name=testuser; datasnek-id=abc123" \
	     --data @settings.json
