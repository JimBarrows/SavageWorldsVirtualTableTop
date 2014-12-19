package org.savageworlds.game.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SetAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2014-12-18T06:47:28.009-0700")
@StaticMetamodel(EdgeDescription.class)
public class EdgeDescription_ {
	public static volatile SingularAttribute<EdgeDescription, Long> id;
	public static volatile SingularAttribute<EdgeDescription, EdgeType> edgeType;
	public static volatile SingularAttribute<EdgeDescription, String> name;
	public static volatile SingularAttribute<EdgeDescription, RankType> minimumRank;
	public static volatile SingularAttribute<EdgeDescription, CharacterType> requiredType;
	public static volatile SetAttribute<EdgeDescription, Skill> minimumSkills;
	public static volatile SetAttribute<EdgeDescription, EdgeDescription> requiredEdges;
}
