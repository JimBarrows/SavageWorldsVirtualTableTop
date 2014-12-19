package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.006-0700")
@StaticMetamodel(Die.class)
public class Die_ {
	public static volatile SingularAttribute<Die, Integer> number;
	public static volatile SingularAttribute<Die, DiceType> dice;
	public static volatile SingularAttribute<Die, Integer> bonus;
}
