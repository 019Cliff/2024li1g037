class_name AccountImportService
extends RefCounted

const NATIVE_SCHEMA := "immutable-towers-account-export"
const TRANSFER_SCHEMA := "immutable-towers-transfer"
const SUPPORTED_VERSION := 1

static func classify(document: Dictionary) -> Dictionary:
	var schema := str(document.get("schema", ""))
	var version := int(document.get("version", 0))
	if version != SUPPORTED_VERSION:
		return {
			"status": "invalid",
			"errors": ["Versao de backup nao suportada"]
		}
	match schema:
		NATIVE_SCHEMA:
			var account: Variant = document.get("exported_account")
			if not account is Dictionary:
				return {
					"status": "invalid",
					"errors": ["O backup nao contem uma conta valida"]
				}
			return {
				"status": "ready",
				"kind": "native_account",
				"account": account.duplicate(true)
			}
		TRANSFER_SCHEMA:
			return {
				"status": "ready",
				"kind": "haskell_transfer"
			}
		_:
			return {
				"status": "invalid",
				"errors": ["Formato de backup desconhecido"]
			}
