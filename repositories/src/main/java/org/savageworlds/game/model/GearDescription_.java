package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import jdo.model.BasePersistentModel_;

@Generated(value="Dali", date="2014-12-24T17:34:47.684-0700")
@StaticMetamodel(GearDescription.class)
public class GearDescription_ extends BasePersistentModel_ {
	public static volatile SingularAttribute<GearDescription, String> name;
	public static volatile SingularAttribute<GearDescription, Long> weight;
	public static volatile SingularAttribute<GearDescription, Long> cost;
	public static volatile SingularAttribute<GearDescription, String> notes;
	public static volatile SingularAttribute<GearDescription, EraType> era;
}
