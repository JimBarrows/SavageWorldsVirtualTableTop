package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import jdo.model.BasePersistentModel_;

@Generated(value="Dali", date="2015-01-07T10:36:41.627-0700")
@StaticMetamodel(EdgeDescription.class)
public class EdgeDescription_ extends BasePersistentModel_ {
	public static volatile SingularAttribute<EdgeDescription, EdgeType> edgeType;
	public static volatile SingularAttribute<EdgeDescription, String> name;
	public static volatile SingularAttribute<EdgeDescription, RankType> minimumRank;
	public static volatile SingularAttribute<EdgeDescription, CharacterType> requiredType;
	public static volatile ListAttribute<EdgeDescription, Skill> minimumSkills;
	public static volatile ListAttribute<EdgeDescription, EdgeDescription> requiredEdges;
}
