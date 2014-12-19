package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.024-0700")
@StaticMetamodel(Round.class)
public class Round_ {
	public static volatile SingularAttribute<Round, RoundType> round;
	public static volatile SingularAttribute<Round, Die> dice;
	public static volatile SingularAttribute<Round, Integer> armorPierce;
	public static volatile SingularAttribute<Round, BurstTemplateType> template;
}
