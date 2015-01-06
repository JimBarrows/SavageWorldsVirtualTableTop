package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SetAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import jdo.model.BasePersistentModel_;

@Generated(value="Dali", date="2015-01-03T15:02:32.764-0700")
@StaticMetamodel(Power.class)
public class Power_ extends BasePersistentModel_ {
	public static volatile SingularAttribute<Power, String> name;
	public static volatile SingularAttribute<Power, String> description;
	public static volatile SingularAttribute<Power, RankType> rank;
	public static volatile SingularAttribute<Power, Integer> powerPoints;
	public static volatile SingularAttribute<Power, String> range;
	public static volatile SingularAttribute<Power, String> duration;
	public static volatile SetAttribute<Power, Trapping> trappings;
}
