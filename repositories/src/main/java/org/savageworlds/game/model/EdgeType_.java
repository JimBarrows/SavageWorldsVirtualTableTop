package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import jdo.model.BasePersistentModel_;

@Generated(value="Dali", date="2015-01-01T08:41:28.413-0700")
@StaticMetamodel(EdgeType.class)
public class EdgeType_ extends BasePersistentModel_ {
	public static volatile SingularAttribute<EdgeType, String> name;
	public static volatile SingularAttribute<EdgeType, String> description;
	public static volatile ListAttribute<EdgeType, EdgeDescription> edges;
}
