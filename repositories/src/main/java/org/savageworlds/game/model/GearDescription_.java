package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.013-0700")
@StaticMetamodel(GearDescription.class)
public class GearDescription_ {
	public static volatile SingularAttribute<GearDescription, Long> id;
	public static volatile SingularAttribute<GearDescription, String> name;
	public static volatile SingularAttribute<GearDescription, Long> weight;
	public static volatile SingularAttribute<GearDescription, Long> cost;
	public static volatile SingularAttribute<GearDescription, String> notes;
	public static volatile SingularAttribute<GearDescription, EraType> era;
}
