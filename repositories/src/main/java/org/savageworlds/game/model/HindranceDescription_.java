package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import jdo.model.BasePersistentModel_;

@Generated(value="Dali", date="2015-01-03T15:02:32.757-0700")
@StaticMetamodel(HindranceDescription.class)
public class HindranceDescription_ extends BasePersistentModel_ {
	public static volatile SingularAttribute<HindranceDescription, String> name;
	public static volatile SingularAttribute<HindranceDescription, HindranceSeverity> severity;
	public static volatile SingularAttribute<HindranceDescription, String> effects;
}
