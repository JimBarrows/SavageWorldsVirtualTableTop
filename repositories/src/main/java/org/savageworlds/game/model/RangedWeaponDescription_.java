package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.023-0700")
@StaticMetamodel(RangedWeaponDescription.class)
public class RangedWeaponDescription_ extends GearDescription_ {
	public static volatile SingularAttribute<RangedWeaponDescription, Integer> shortRange;
	public static volatile SingularAttribute<RangedWeaponDescription, Integer> mediumRange;
	public static volatile SingularAttribute<RangedWeaponDescription, Integer> longRange;
	public static volatile SingularAttribute<RangedWeaponDescription, AttributeTypes> attribute;
	public static volatile SingularAttribute<RangedWeaponDescription, DiceType> dice;
	public static volatile SingularAttribute<RangedWeaponDescription, Integer> number;
	public static volatile SingularAttribute<RangedWeaponDescription, Integer> bonus;
	public static volatile SingularAttribute<RangedWeaponDescription, Integer> rateOfFire;
	public static volatile SingularAttribute<RangedWeaponDescription, Integer> shots;
	public static volatile SingularAttribute<RangedWeaponDescription, DiceType> minStrength;
}
