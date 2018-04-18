package org.savageworlds.vtt.virtualtabletopapi.models;

public enum Dice {
	d4(4), d6(6), d8(8), d10(10), d12(12);

	private int value;

	Dice(int value) {
		this.value = value;
	}

	public int getValue() {
		return value;
	}
}
