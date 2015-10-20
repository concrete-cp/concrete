package concrete.runner

import java.awt.Color
import java.io.File
import org.gephi.data.attributes.api.AttributeController
import org.gephi.graph.api.GraphController
import org.gephi.io.exporter.api.ExportController
import org.gephi.layout.plugin.force.StepDisplacement
import org.gephi.layout.plugin.force.yifanHu.YifanHuLayout
import org.gephi.preview.api.PreviewController
import org.gephi.preview.api.PreviewProperty
import org.gephi.preview.types.EdgeColor
import org.gephi.project.api.ProjectController
import org.gephi.ranking.api.Ranking
import org.gephi.ranking.api.RankingController
import org.gephi.ranking.api.Transformer
import org.gephi.ranking.plugin.transformer.AbstractColorTransformer
import org.gephi.ranking.plugin.transformer.AbstractSizeTransformer
import org.gephi.statistics.plugin.GraphDistance
import org.openide.util.Lookup
import cspom.CSPOM
import cspom.VariableNames
import cspom.variable.CSPOMVariable
import org.gephi.clustering.plugin.mcl.MarkovClustering
import de.uni_leipzig.informatik.asv.gephi.chinesewhispers.ChineseWhispersClusterer
import org.gephi.layout.plugin.fruchterman.FruchtermanReingold
import org.gephi.statistics.plugin.Modularity
import org.gephi.partition.api.PartitionController
import org.gephi.partition.plugin.NodeColorTransformer
import org.gephi.partition.api.Partition
import org.gephi.layout.plugin.openord.OpenOrdLayout

object Gephi {

  def apply(cspom: CSPOM, to: File): Unit = {

    //Init a project - and therefore a workspace
    val pc = Lookup.getDefault().lookup(classOf[ProjectController]);
    pc.newProject();
    val workspace = pc.getCurrentWorkspace();

    val attributeModel = Lookup.getDefault().lookup(classOf[AttributeController]).getModel();
    val graphModel = Lookup.getDefault().lookup(classOf[GraphController]).getModel();
    val model = Lookup.getDefault().lookup(classOf[PreviewController]).getModel();

    val rankingController = Lookup.getDefault().lookup(classOf[RankingController]);

    val vn = new VariableNames(cspom)
    val vars = cspom.referencedExpressions
      .collect {
        case e: CSPOMVariable[_] => e -> graphModel.factory().newNode(vn.names(e))
      }
      .toMap

    val drawnConstraints = cspom.constraints.toSeq//filter(_.function == 'sum).toSeq

    val constraints = drawnConstraints.zipWithIndex.map {
      case (c, i) =>
        val n = graphModel.factory().newNode(s"c$i")
        n.getNodeData.setLabel(c.function.name)
        c -> n
    }
      .toMap

    val edges = drawnConstraints.flatMap { c =>
      val n = constraints(c)
      c.fullScope.flatMap(_.flatten).collect {
        case v: CSPOMVariable[_] =>
          graphModel.factory().newEdge(n, vars(v))
      }
    }

    val graph = graphModel.getUndirectedGraph()
    vars.values.foreach(graph.addNode)
    constraints.values.foreach(graph.addNode)
    edges.foreach(graph.addEdge)

    val partitionController = Lookup.getDefault().lookup(classOf[PartitionController]);

    val modularity = new Modularity()
    
    modularity.execute(graphModel, attributeModel)

    val modColumn = attributeModel.getNodeTable().getColumn(Modularity.MODULARITY_CLASS);
    val p2 = partitionController.buildPartition(modColumn, graph);
    System.out.println(p2.getPartsCount() + " partitions found");
    val nodeColorTransformer2 = new NodeColorTransformer();
    nodeColorTransformer2.randomizeColors(p2);
    partitionController.transform(p2, nodeColorTransformer2);

    val layout = new OpenOrdLayout(null) //, new StepDisplacement(1f))
    layout.setGraphModel(graphModel);

    layout.resetPropertiesValues();

    //layout.setOptimalDistance(200f);
    layout.initAlgo();
    while (layout.canAlgo()) layout.goAlgo()
    //    for (i <- 0 until 1000 if layout.canAlgo()) {
    //      layout.goAlgo();
    //    }
    layout.endAlgo();

    layout.getGraph

    //Preview
    model.getProperties().putValue(PreviewProperty.SHOW_NODE_LABELS, true);
    model.getProperties().putValue(PreviewProperty.EDGE_COLOR, new EdgeColor(Color.GRAY));
    model.getProperties().putValue(PreviewProperty.EDGE_THICKNESS, 0.1f);
    model.getProperties().putValue(PreviewProperty.NODE_LABEL_FONT, model.getProperties().getFontValue(PreviewProperty.NODE_LABEL_FONT).deriveFont(8));

    //Export
    val ec = Lookup.getDefault().lookup(classOf[ExportController]);

    ec.exportFile(to);
  }
}