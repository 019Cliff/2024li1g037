class_name TowerEconomy
extends RefCounted

const REFUND_RATE := 0.65

static func purchase_price(base_price: int, mode_id: String) -> int:
	var discount := 24 if mode_id == "sandbox" else (12 if mode_id == "challenge" else 0)
	return maxi(12, base_price - discount)

static func upgrade_cost_values(damage: float, range_cells: float, burst: int, cycle: float) -> int:
	return floori(28.0 + damage * 2.15 + range_cells * 10.0 + float(burst * 22) + (2.2 - minf(2.0, cycle)) * 42.0)

static func upgrade_cost(tower: Variant, specialization_choice: String = "") -> int:
	if tower.level >= tower.max_level:
		return -1
	var base_cost := upgrade_cost_values(tower.damage, tower.range_cells, tower.burst, tower.cycle)
	if tower.level >= 3 and tower.specialization.is_empty():
		if specialization_choice == "a":
			return ceili(float(base_cost) * 1.18)
		if specialization_choice == "b":
			return ceili(float(base_cost) * 1.12)
		return -1
	return base_cost

static func sale_value(tower: Variant, _initial_base: Dictionary = {}) -> int:
	return refund_for_investment(int(tower.investment_paid))

static func refund_for_investment(investment_paid: int) -> int:
	if investment_paid <= 0:
		return 0
	return clampi(floori(float(investment_paid) * REFUND_RATE), 0, investment_paid)
