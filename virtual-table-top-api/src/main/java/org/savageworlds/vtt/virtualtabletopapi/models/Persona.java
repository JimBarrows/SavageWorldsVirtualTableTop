package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.PositiveOrZero;
import java.util.LinkedHashSet;
import java.util.Set;

@Entity
public class Persona extends Creature {

	@ManyToOne
	private PlotPoint plotPoint;
	@Enumerated
	private Rank rank = Rank.novice;
	@PositiveOrZero
	private int experiencePoints = 0;
	@PositiveOrZero
	private int powerPoints = 0;
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@OrderBy(value = "hindrance.name, notes")
	private Set<PersonaHindrance> hindrances = new LinkedHashSet<PersonaHindrance>();
	@ManyToMany(fetch = FetchType.EAGER)
	@OrderBy("name")
	private Set<Edge> edges = new LinkedHashSet<>();
	@ManyToMany(fetch = FetchType.EAGER)
	@OrderBy("power.name, trapping")
	private Set<PersonaPower> powers = new LinkedHashSet<PersonaPower>();
	@ManyToMany(fetch = FetchType.EAGER)
	@OrderBy("name")
	private Set<Gear> gear = new LinkedHashSet<>();

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("rank", rank)
				.append("experiencePoints", experiencePoints)
				.append("powerPoints", powerPoints)
				.append("hindrances", hindrances)
				.append("edges", edges)
				.append("powers", powers)
				.append("gear", gear)
				.toString();
	}

	public PlotPoint getPlotPoint() {
		return plotPoint;
	}

	public void setPlotPoint(final PlotPoint plotPoint) {
		this.plotPoint = plotPoint;
	}

	public int getPowerPoints() {
		return powerPoints;
	}

	public void setPowerPoints(final int powerPoints) {
		this.powerPoints = powerPoints;
	}

	public int getExperiencePoints() {
		return experiencePoints;
	}

	public void setExperiencePoints(final int experiencePoints) {
		this.experiencePoints = experiencePoints;
	}

	public Rank getRank() {
		return rank;
	}

	public void setRank(final Rank rank) {
		this.rank = rank;
	}

	public Set<PersonaHindrance> getHindrances() {
		return hindrances;
	}

	public void setHindrances(final Set<PersonaHindrance> hindrances) {
		this.hindrances = hindrances;
	}

	public Set<Edge> getEdges() {
		return edges;
	}

	public void setEdges(final Set<Edge> edges) {
		this.edges = edges;
	}

	public Set<PersonaPower> getPowers() {
		return powers;
	}

	public void setPowers(final Set<PersonaPower> powers) {
		this.powers = powers;
	}

	public Set<Gear> getGear() {
		return gear;
	}

	public void setGear(final Set<Gear> gear) {
		this.gear = gear;
	}
}
