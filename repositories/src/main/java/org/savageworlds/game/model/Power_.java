package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SetAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.019-0700")
@StaticMetamodel(Power.class)
public class Power_ {
	public static volatile SingularAttribute<Power, Long> id;
	public static volatile SingularAttribute<Power, String> name;
	public static volatile SingularAttribute<Power, String> description;
	public static volatile SingularAttribute<Power, RankType> rank;
	public static volatile SingularAttribute<Power, Integer> powerPoints;
	public static volatile SingularAttribute<Power, String> range;
	public static volatile SingularAttribute<Power, String> duration;
	public static volatile SetAttribute<Power, Trapping> trappings;
}
