package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SetAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import jdo.model.BasePersistentModel_;

@Generated(value="Dali", date="2015-01-01T08:37:56.091-0700")
@StaticMetamodel(EdgeDescription.class)
public class EdgeDescription_ extends BasePersistentModel_ {
	public static volatile SingularAttribute<EdgeDescription, EdgeType> edgeType;
	public static volatile SingularAttribute<EdgeDescription, String> name;
	public static volatile SingularAttribute<EdgeDescription, RankType> minimumRank;
	public static volatile SingularAttribute<EdgeDescription, CharacterType> requiredType;
	public static volatile SetAttribute<EdgeDescription, Skill> minimumSkills;
	public static volatile SetAttribute<EdgeDescription, EdgeDescription> requiredEdges;
}
